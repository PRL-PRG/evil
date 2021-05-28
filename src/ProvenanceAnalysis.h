#ifndef EVIL_PROVENANCE_ANALYSIS_H
#define EVIL_PROVENANCE_ANALYSIS_H

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
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
        std::unordered_map<SEXP, std::tuple<ProvenanceKind, std::string, int> > addresses;
        // to store multiple provenances
        size_t set_size;
        const static size_t nb_kinds = static_cast<int>(ProvenanceKind::match_call) + 1;
        std::vector<int> kind;
        std::unordered_set<std::string> args;
        std::unordered_set<int> provenances;
    public:
        ProvenanceAnalysis(): Analysis(),
         set_size(10),
         kind(nb_kinds), args(set_size), provenances(set_size) {}

        void analyze(TracerState& tracer_state, Event& event) override {
            Event::Type event_type = event.get_type();
            Stack& stack = tracer_state.get_stack();

            if(event_type == Event::Type::ClosureCallExit || event_type == Event::Type::SpecialCallExit) {
                const StackFrame& frame = stack.peek();
                const Call* call = frame.as_call();
                const Function* function = call->get_function();

                if(function->has_identity(Function::Identity::ProvenanceFamily)) {
                    // Get the return value
                    SEXP result = event.get_result();
                    Rprintf("Result is %s, with address %p, with type %s\n", 
                        deparse(result, call->get_environment()).c_str(),
                        &result,
                        CHAR(STRING_ELT(sexp_typeof(result), 0)));

                    ProvenanceKind provenance =  ProvenanceTable::identity_to_provenance(function->get_identity());
                    std::string arguments = deparse(call->get_expression(), call->get_environment());
                    
                    // For one provenance, one provenance id
                    // i.e. one provenance = one call, not one site
                    auto payload = std::make_tuple(provenance, arguments, get_provenance_id());

                    // Get address of the value or of the elements if it is a complex expression

                    switch (TYPEOF(result))
                    {
                    case STRSXP: // That should not happen!
                        for(int i = 0; i < XLENGTH(result); i++) {
                            SEXP el = STRING_ELT(result, i);
                            addresses[el] = payload ;
                        }
                        break;

                    case VECSXP:
                    case EXPRSXP: // similar internal representation
                        //Rprintf("Detecting vector or expression\n");
                        for(int i = 0; i < XLENGTH(result); i++) {
                            SEXP el = VECTOR_ELT(result, i);
                            addresses[el] = payload;
                        }
                        break;

                    case LISTSXP:
                    case LANGSXP: {
                        //Rprintf("Detecting language or pairlist\n");
                        for(SEXP cons = result; cons != R_NilValue; cons = CDR(cons)) {
                            SEXP el = CAR(cons);
                            // Also add cons in the addresses?
                            //Rprintf("Adding address %p with SEXP %s\n", el, deparse(el, call->get_environment()));
                            addresses[el] = payload;
                            addresses[cons] = payload;
                        } }
                        break;
                    
                    default: // simple values // that should not happen
                        addresses[result] = payload;
                        break;
                    }

                    Rprintf("Detected provenance function %s\n", arguments.c_str());
                }

                if(function->has_identity(Function::Identity::EvalFamily)) {
                    Rprintf("Now in eval! We have %d addresses recorded\n", addresses.size());
                    
                    // for(auto it = addresses.cbegin(); it != addresses.cend(); it++) {
                    //     Rprintf("Address %p with arguments %s and provenance id %d\n", 
                    //     it->first, std::get<1>(it->second).c_str(),
                    //     std::get<2>(it->second));
                    // }
                    // Check if the expression address contains any address saved previously.
                    SEXP expr_promise = event.r_get_argument(Rf_install("expr"), 0);
                    SEXP expr_arg = dyntrace_get_promise_value(expr_promise);
                    Rprintf("New address %p for %s\n", &expr_arg, deparse(expr_arg, call->get_environment()).c_str());

                    

                    // multiple provenances
                    clear_sets();

                    auto res = addresses.find(expr_arg);
                    
                    if(res == addresses.end()) {// Not found
                        // try to look inside
                        switch(TYPEOF(expr_arg)) {
                            case STRSXP:
                            for(int i = 0; i < XLENGTH(expr_arg); i++) {
                                SEXP el = STRING_ELT(expr_arg, i);
                                res = addresses.find(el);
                                if(res != addresses.end()) {
                                  update_provenances(res->second);
                                }
                            }
                            break;

                            case VECSXP:
                            case EXPRSXP:
                            for(int i = 0; i < XLENGTH(expr_arg); i++) {
                                SEXP el = VECTOR_ELT(expr_arg, i);
                                res = addresses.find(el);
                                if(res != addresses.end()) {
                                   update_provenances(res->second);
                                }
                            }
                            break;

                            // Her,e we could even traverse further into
                            // the tree of the SEXP, and not only visit 
                            // the roots
                            case LISTSXP:
                            case LANGSXP:
                            for(SEXP cons = expr_arg; cons != R_NilValue; cons = CDR(cons)) {
                                SEXP el = CAR(cons);
                                // Also add the cons address?
                                res = addresses.find(el);
                                if(res != addresses.end()) {
                                   update_provenances(res->second);
                                }
                                res = addresses.find(cons);
                                if(res != addresses.end()) {
                                 update_provenances(res->second);
                                }
                            }
                            break;
                        }
                        
                        if(res == addresses.end()) {
                            Rprintf("Not found\n");
                            return;
                        }
                    }
                    else { //Found
                      update_provenances(res->second);
                    }

                    std::string arg_str;
                    for(auto it = args.cbegin(); it != args.cend(); it++) {
                        arg_str += *it + " ; ";
                    }
                    // if yes, record
                    provenance_table_.record(call->get_id(),
                        get_representative(),
                        arg_str.c_str(),
                        provenances.size()); 
                        

                    Rprintf("Detected origin of expression: %s\n", std::get<1>(res->second).c_str());
                }
            }
            else if(event_type == Event::Type::GcUnmark) {
                // if the SEXP is reclaimed by the GC, we can remove it from
                // the hash table

                addresses.erase(event.get_object);
            }
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

    void clear_sets() {
        set_size = std::max(set_size,kind.size(), provenances.size(), args.size());
        provenances.clear();
        args.clear();
        
        std::fill(kind.begin(), kind.end(), 0);
        provenances.reserve(set_size);
        args.reserve(set_size);
    }

    void insert_kind(ProvenanceKind k) {
        kind[static_cast<int>(k)] += 1;
    }

    ProvenanceKind get_representative() const {
        return *max_element(kind.cbegin(), kind.cend());
    }

    void update_provenances(std::tuple<ProvenanceKind, std::string, int>) {
        insert_kind(std::get<0>(res->second));
        args.insert(std::get<1>(res->second));
        provenances.insert(std::get<2>(res->second));
    }

};

#endif