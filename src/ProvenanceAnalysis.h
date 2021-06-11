#ifndef EVIL_PROVENANCE_ANALYSIS_H
#define EVIL_PROVENANCE_ANALYSIS_H

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <tuple>
#include <algorithm>
#include <optional>
#include <utility>
#include <cassert>
#include "r_init.h"
#include "Analysis.h"
#include "ProvenanceTable.h"
#include "Provenance.h"

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
    ProvenanceGraph provenance_graph_;
    ProvenanceTable provenance_table_;
    std::unordered_map<SEXP, Provenance*> addresses; // or unordered_multimap?
  public:
    ProvenanceAnalysis(): Analysis(){
    }

    void analyze(TracerState& tracer_state, Event& event) override {
        Event::Type event_type = event.get_type();
        Stack& stack = tracer_state.get_stack();

        if (event_type == Event::Type::ClosureCallExit ||
            event_type == Event::Type::SpecialCallExit) {
            const Call* call = stack.peek_call(0);
            const Function* function = call->get_function();

            // Get the return value
            SEXP result = event.get_result();

            if (function->has_identity(Function::Identity::ProvenanceFamily) ||
                (result != nullptr &&
                 (TYPEOF(result) == LANGSXP || TYPEOF(result) == EXPRSXP ||
                  TYPEOF(result) == SYMSXP))) {
                // That does not detect if somewhere in the provenance chain, something is not an expression
                // anymore.  For instance, a match.call and non-symbolic arguments

                // Rprintf("Result is %s, with address %p, with type %s\n",
                //     deparse(result, call->get_environment()).c_str(),
                //     &result,
                //     CHAR(STRING_ELT(sexp_typeof(result), 0)));

                std::string function_name = function->get_name();

                std::string full_call =
                    deparse(call->get_expression());

                Provenance* payload = provenance_graph_.add_node(
                    result, function_name, full_call, get_provenance_id());



                // Rprintf("Detected provenance function %s\n",
                // arguments.c_str());

                // TODO: see what happens when the function is [[<-

                // Now, also check if the argument of the expression had also
                // been recorded in the table
                SEXP args = CDR(call->get_expression());
                for (SEXP cons = args; cons != R_NilValue; cons = CDR(cons)) {
                    SEXP expr_arg = CAR(cons);
                    if(TYPEOF(expr_arg) == PROMSXP) {
                        expr_arg = dyntrace_get_promise_value(expr_arg);
                        if(expr_arg == R_UnboundValue) {
                            continue;
                        }
                    }
                   
                     
                    // Here, we need to traverse each of the argument and check
                    // if they are in the hash table if yes, we can add the
                    // provenance(s) of this argument as parents of the current
                    // provenance
                    auto res = addresses.find(expr_arg);

                    // We could also recursively traverse the argument if it is a vec
                    // to find possible addresses
                    // But I think one layer deep is enough
                    if (res != addresses.end()) {
                        payload->add_parent(res->second);
                    }
                    else if(expr_arg != nullptr && TYPEOF(expr_arg) == VECSXP) { 
                        for (int i = 0; i < XLENGTH(expr_arg); i++) {
                            SEXP el = VECTOR_ELT(expr_arg, i);
                            res = addresses.find(el);
                            if (res != addresses.end()) {
                                payload->add_parent(res->second);
                            }
                        }
                    }
                }

                // And also check if the result has already been stored in the table!
                // That would be a return value for instance
                // e.g. g <- function() { parse(text = "1")}
                auto res = addresses.find(result);
                if(res != addresses.end()) {
                    payload->add_parent(res->second, true);
                }

                if(function_name == "[[") { // and $ and [ ?
                    SEXP r_value = Rf_findVarInFrame(call->get_environment(), CAR(args));
                    res = addresses.find(r_value);
                    if(res != addresses.end()) {
                        payload->add_parent(res->second);
                    }
                }

                // We add the new payload after (otherwise, it could shadow the address of one of the arguments)
                addresses[result] = payload;
            }

            if (function->has_identity(Function::Identity::EvalFamily)) {
                //  Rprintf("Now in eval! We have %d addresses recorded\n",
                //  addresses.size());

                // for(auto it = addresses.cbegin(); it != addresses.cend();
                // it++) {
                //     Rprintf("Address %p with arguments %s and provenance id
                //     %d\n", it->first, std::get<1>(it->second).c_str(),
                //     std::get<2>(it->second));
                // }
                // Check if the expression address contains any address saved
                // previously.
                SEXP expr_promise = event.r_get_argument(Rf_install("expr"), 0);
                SEXP expr_arg = dyntrace_get_promise_value(expr_promise);
                // Rprintf("New address %p for %s\n", expr_arg,
                // deparse(expr_arg, call->get_environment()).c_str());

                assert(call->get_id() != NA_INTEGER);

                auto res = addresses.find(expr_arg);

                if (res != addresses.end()) {
                    Provenance* prov = res->second;
                    provenance_table_.record(call->get_id(),
                                             prov->get_representative()->get_name(),
                                             // TODO: handle \n in the full call
                                             prov->get_representative()->get_full_call(),
                                             prov->nb_roots(),
                                             prov->nb_nodes(),
                                             prov->longest_path());
                    //  Rprintf("Detected origin of expression: %s\n",
                    //  arg_str.c_str());
                    std::string filename = function->get_name() + "-" + std::to_string(call->get_id()) + ".dot";
                    ProvenanceGraph::toDot(filename, call->get_id(), prov);

                } 
            }
        } else if (event_type == Event::Type::GcUnmark) {
            // if the SEXP is reclaimed by the GC, we can remove it from
            // the hash table
            // It makes the analysis slightly more correct, by preventing
            // from happening
            // the rare case of an unrelated SEXP allocated at the same address
            // as a reclaimed address that had been recorded previously

            addresses.erase(event.get_object());
        } else if (event_type == Event::Type::BuiltinCallExit) {
            // Desactivated currently.
            // It seems that when activated, we no longer detect ClosureExit and
            // SpecialExit...
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            const Function* function = call->get_function();
            Rprintf("Now in builtin %s with expression %s\n",
                    function->get_name().c_str(),
                    deparse(call->get_expression())
                        .c_str());
        }
    }

    std::vector<Table*> get_tables() override {
        return {&provenance_table_};
    }

    std::string deparse(const SEXP expr) {
        std::string deparsed = "ERROR";
        SEXP res;

        PROTECT(res =
                    Rf_deparse1(expr, FALSE, KEEPINTEGER | KEEPNA | DIGITS17));
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