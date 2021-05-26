#ifndef EVIL_PROVENANCE_ANALYSIS_H
#define EVIL_PROVENANCE_ANALYSIS_H

#include <vector>
#include <string>
#include <unordered_map>
#include <tuple>
#include <utility> 
#include "r_init.h"
#include "Analysis.h"
#include "ProvenanceTable.h"

// deparse options
#define KEEPINTEGER 1
#define QUOTEEXPRESSIONS 2
#define SHOWATTRIBUTES 4
#define USESOURCE 8
#define WARNINCOMPLETE 16
#define DELAYPROMISES 32
#define KEEPNA 64
#define S_COMPAT 128
#define HEXNUMERIC 256
#define DIGITS17 512
#define NICE_NAMES 1024
extern "C" {
    SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);
}


class ProvenanceAnalysis: public Analysis {
    private:
        inline static int provenance_id = 0;
        ProvenanceTable provenance_table_;
        // Kind of provenance function, argument list,
        // unique id of the provenance
        std::unordered_map<SEXP*, std::tuple<ProvenanceKind, std::string, int> > addresses;
    public:
        ProvenanceAnalysis(): Analysis() {}

        void analyze(TracerState& tracer_state, Event& event) override {
            Event::Type event_type = event.get_type();
            Stack& stack = tracer_state.get_stack();

            if(event_type == Event::Type::ClosureCallExit) {
                const StackFrame& frame = stack.peek();
                const Call* call = frame.as_call();
                const Function* function = call->get_function();

                if(function->has_identity(Function::Identity::ProvenanceFamily)) {
                    // Get the return value
                    SEXP result = event.get_result();
                    Rprintf("Result is %s, with address %p\n", 
                        deparse(result, call->get_environment()).c_str(),
                        &result);

                    ProvenanceKind provenance =  ProvenanceTable::identity_to_provenance(function->get_identity());
                    std::string arguments = deparse(call->get_expression(), call->get_environment());
                    
                    // For one provenance, one provenance id
                    auto payload = std::make_tuple(provenance, arguments, get_provenance_id());

                    // Get address of the value or of the elements if it is a complex expression

                    switch (TYPEOF(result))
                    {
                    case STRSXP:
                        for(int i = 0; i < XLENGTH(result); i++) {
                            SEXP el = STRING_ELT(result, i);
                            addresses[&el] = payload ;
                        }
                        break;

                    case VECSXP:
                    //case EXPRSXP: // similar internal representation
                        for(int i = 0; i < XLENGTH(result); i++) {
                            SEXP el = VECTOR_ELT(result, i);
                            addresses[&el] = payload;
                        }
                        break;

                    case LISTSXP:
                        for(SEXP cons = result; cons != R_NilValue; cons = CDR(cons)) {
                            SEXP el = CAR(cons);
                            addresses[&el] = payload;
                        }
                        break;
                    
                    default: // simple values
                        addresses[&result] = payload;
                        break;
                    }

                    Rprintf("Detected provenance function %s\n", arguments.c_str());
                }

                if(function->has_identity(Function::Identity::EvalFamily)) {
                    Rprintf("Now in eval! We have %d addresses recorded\n", addresses.size());
                    
                    for(auto it = addresses.cbegin(); it != addresses.cend(); it++) {
                        Rprintf("Address %p with arguments %s\n", it->first, std::get<1>(it->second).c_str());
                    }
                    // Check if the expression address contains any address saved previously.
                    SEXP expr_promise = event.r_get_argument(Rf_install("expr"), 0);
                    SEXP expr_arg = dyntrace_get_promise_value(expr_promise);
                    Rprintf("New address %p for %s\n", &expr_arg, deparse(expr_arg, call->get_environment()).c_str());

                    auto res = addresses.find(&expr_arg);
                    
                    if(res == addresses.end()) {// Not found
                        // try to look inside
                        Rprintf("Looking inside the expression\n");
                        switch(TYPEOF(expr_arg)) {
                            case STRSXP:
                            for(int i = 0; i < XLENGTH(expr_arg); i++) {
                                SEXP el = STRING_ELT(expr_arg, i);
                                res = addresses.find(&el);
                                if(res != addresses.end()) {
                                    break;
                                }
                            }
                            break;

                            case VECSXP:
                            case EXPRSXP:
                            for(int i = 0; i < XLENGTH(expr_arg); i++) {
                                SEXP el = VECTOR_ELT(expr_arg, i);
                                res = addresses.find(&el);
                                if(res != addresses.end()) {
                                    break;
                                }
                            }
                            break;

                            case LISTSXP:
                            for(SEXP cons = expr_arg; cons != R_NilValue; cons = CDR(cons)) {
                                SEXP el = CAR(cons);
                                res = addresses.find(&el);
                                if(res != addresses.end()) {
                                    break;
                                }
                            }
                            break;
                        }
                        
                        if(res == addresses.end()) {
                            Rprintf("Not found\n");
                            return;
                        }
                    }
                    // if yes, record
                    //TODO: count the number of provenances
                    provenance_table_.record(call->get_id(),
                        std::get<0>(res->second),
                        std::get<1>(res->second).c_str(),
                        1); 
                        

                    Rprintf("Detected origin of expression: %s", std::get<1>(res->second).c_str());

                    // TODO: the expression can be composite and each part of it could possibly
                    // come from different origins
                }
            }

            // Probably add a hook for GC desallocation, to remove for our table

        }

    std::vector<Table*> get_tables() override {
        return {&provenance_table_};
    }

    std::string deparse(const SEXP expr, const SEXP env) {
        std::string deparsed = "ERROR";
        SEXP res;

        PROTECT(res = Rf_deparse1(expr, FALSE, KEEPINTEGER | KEEPNA | DIGITS17));
        deparsed = CHAR(STRING_ELT(res, 0));
        UNPROTECT(1);
        return deparsed;
    }

private:

    static int get_provenance_id() {
        return ++provenance_id;
    }

};

#endif