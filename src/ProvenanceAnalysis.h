#ifndef EVIL_PROVENANCE_ANALYSIS_H
#define EVIL_PROVENANCE_ANALYSIS_H

#include <vector>
#include <string>
#include <unordered_map>
#include <utility> 
#include "r_init.h"
#include "Analysis.h"
#include "ProvenanceTable.h"





class ProvenanceAnalysis: public Analysis {
    private:
        ProvenanceTable provenance_table_;
        // or rather a tuple with type and arguments?
        std::unordered_map<SEXP*, std::pair<ProvenanceKind, std::string> > addresses;
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

                    ProvenanceKind provenance =  ProvenanceTable::identity_to_provenance(function->get_identity());
                    std::string arguments = deparse(call->get_expression(), call->get_environment());

                    auto payload = std::make_pair(provenance, arguments);

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
                    case EXPRSXP: // similar internal representation
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
                }

                if(function->has_identity(Function::Identity::EvalFamily)) {
                    // Check if the expression address contains any address saved previously.
                    SEXP expr_arg = event.r_get_argument(Rf_install("expr"), 0);
                    auto res = addresses.find(&expr_arg);
                    
                    if(res == addresses.end()) {// Not found
                        return;
                    }
                    // if yes, record
                    provenance_table_.record(call->get_id(),
                        res->second.first,
                        res->second.second);

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
        SEXP call = PROTECT(lang2(install("deparse1"), expr));
        int error; 
        SEXP res;
        PROTECT(res = R_tryEval(call, env, &error));
       
        if(!error) {
            deparsed = CHAR(STRING_ELT(res, 0));
        }
        UNPROTECT(2);
        return deparsed;
    }

};

#endif