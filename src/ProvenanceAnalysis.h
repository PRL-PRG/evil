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
#include <cstdlib>
#include "r_init.h"
#include "r_utilities.h"
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
    std::unordered_map<SEXP, Provenance*> addresses;
    std::unordered_set<std::string> unique_provenances;

    // To remember the provenance
    // in the case f()[[1]]
    std::optional<SEXP> eval_sexp;
    Provenance* eval_provenance;

    // also add "{" ?
    // it helps capturing everything created by an interesting function, even if
    // it is a realsxp e.g. from quote(1) We can remove if we are only
    // interested in EXPRSXP, LANGSXP and so on
    inline static std::unordered_set<std::string> rw_functions =
        {"[[", "[", "$", "<-", "[[<-", "[<-", "$<-"};

  public:
    ProvenanceAnalysis()
        : Analysis(), unique_provenances(Provenance::nb_special_functions()) {
    }

    void analyze(TracerState& tracer_state, Event& event) override {
        Event::Type event_type = event.get_type();
        Stack& stack = tracer_state.get_stack();

        if(!r_tracing){
            return;
        }

        if (event_type == Event::Type::ClosureCallExit ||
            event_type == Event::Type::SpecialCallExit ||
            event_type == Event::Type::BuiltinCallExit) {
            const Call* call = stack.peek_call(0);
            const Function* function = call->get_function();

            // Get the return value
            SEXP result = event.get_result();
            std::string function_name = function->get_name();

            // Also track list with at least one langsxp inside
            // Maybe we should track better assignment rather than taversing the full list
            // it could be huge...
            bool lang_list = false;
            if(result != nullptr && TYPEOF(result) == VECSXP) {
                if(XLENGTH(result) == 0) {
                    lang_list = true;// we track the empty list
                }
                for(int i = 0; i < XLENGTH(result) ; i++) {
                    int el_type = TYPEOF(VECTOR_ELT(result, i));
                    if(el_type == LANGSXP || el_type == EXPRSXP || el_type == SYMSXP) {
                        lang_list = true;
                        break;
                    }
                }
            }

            if (function->has_identity(Function::Identity::ProvenanceFamily) ||
                // (rw_functions.find(function_name) != rw_functions.end()) ||
                (result != nullptr &&
                 (TYPEOF(result) == LANGSXP || TYPEOF(result) == EXPRSXP ||
                  TYPEOF(result) == SYMSXP || TYPEOF(result) == LISTSXP)) || lang_list) {
                // That does not detect if somewhere in the provenance chain,
                // something is not an expression anymore.  For instance, a
                // match.call and non-symbolic arguments
                // I also added LISTSXP, as it is the return type of functions such as formals

                // Rprintf("Result is %s, with address %p, with type %s\n",
                //     deparse(result, call->get_environment()).c_str(),
                //     &result,
                //     CHAR(STRING_ELT(sexp_typeof(result), 0)));

                std::string full_call = deparse(call->get_expression());

                Provenance* payload = provenance_graph_.add_node(
                    result, function_name, full_call, get_provenance_id());

                // Rprintf("Detected provenance function %s\n",
                // arguments.c_str());


                // Now, also check if the argument of the expression had also
                // been recorded in the table
                SEXP args = CDR(call->get_expression());
                for (SEXP cons = args; cons != R_NilValue; cons = CDR(cons)) {
                    SEXP expr_arg = CAR(cons);
                    if (TYPEOF(expr_arg) == PROMSXP) {
                        expr_arg = dyntrace_get_promise_value(expr_arg);
                        if (expr_arg == R_UnboundValue) {
                            continue;
                        }
                    }

                    // Here, we need to traverse each of the argument and check
                    // if they are in the hash table if yes, we can add the
                    // provenance(s) of this argument as parents of the current
                    // provenance
                    auto res = addresses.find(expr_arg);

                    // We could also recursively traverse the argument if it is
                    // a vec to find possible addresses But I think one layer
                    // deep is enough
                    if (res != addresses.end()) {
                        payload->add_parent(res->second);
                    } else if (expr_arg != nullptr &&
                               TYPEOF(expr_arg) == VECSXP) {
                        for (int i = 0; i < XLENGTH(expr_arg); i++) {
                            SEXP el = VECTOR_ELT(expr_arg, i);
                            res = addresses.find(el);
                            if (res != addresses.end()) {
                                payload->add_parent(res->second);
                            }
                        }
                    }
                }

                // And also check if the result has already been stored in the
                // table! That would be a return value for instance e.g. g <-
                // function() { parse(text = "1")}
                // but don't do it for the subset operators
                // otherwise we would get two parents if an element has been inserted
                // before: the builder of the full expression and the builder of 
                // the inserted element but the inserted element should be parent 
                // of the full expression already
                
                if (function_name == "[[" || function_name == "$" ||
                    function_name == "[") {
                    SEXP lhs = CAR(args);
                    if (TYPEOF(lhs) == SYMSXP) {
                        SEXP r_value =
                            Rf_findVarInFrame(call->get_environment(), lhs);
                        auto res = addresses.find(r_value);
                        if (res != addresses.end()) {
                            payload->add_parent(res->second);
                        }
                    } else if (TYPEOF(lhs) == LANGSXP) {
                        // This is tricky: f()[[1]]
                        // There is no new symbol visible to R
                        // R uses do_subset that calls R_DispatchOrEval
                        // This will use the C eval to evaluate f()
                        //
                        // So we also use the eval exit callback and record if
                        // it generated something interesting
                        if (eval_sexp.has_value() && eval_sexp.value() == lhs) {
                            // we need to add to the provenance graph the
                            // provenance recorded in the eval callback
                            assert(eval_provenance != nullptr);
                            payload->add_parent(eval_provenance);
                        }
                    }
                } else {
                    auto res = addresses.find(result);
                    if (res != addresses.end()) {
                        payload->add_parent(res->second, true);
                    }
                }
                
                if(function_name == "list") {
                    // This would not be detected by looking at the arguments
                    // because the arguments of the list are not promises but 
                    // LANGSXP
                    for(int i = 0; i < XLENGTH(result) ; i++) {
                        SEXP el = VECTOR_ELT(result, i);
                        auto res = addresses.find(el);
                        if (res != addresses.end()) {
                            payload->add_parent(res->second);
                        }   
                    }
                }

                // We add the new payload after (otherwise, it could shadow the
                // address of one of the arguments)
                addresses[result] = payload;
            }

            if (function->has_identity(Function::Identity::EvalFamily)) {
                if (call->get_id() == NA_INTEGER) {
                    // It means that it is an eval that we had decided to ignore
                    // So we do not record anything here.
                    return;
                }
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

                auto res = addresses.find(expr_arg);

                if (res != addresses.end()) {
                    Provenance* prov = res->second;

                    auto repr_path = prov->rep_path();

                    provenance_table_.record(
                        call->get_id(),
                        prov->get_representative()->get_name(),
                        prov->get_representative()->get_full_call(),
                        prov->nb_roots(),
                        prov->nb_nodes(),
                        prov->longest_path(),
                        provenances_from_roots(prov),
                        repr_path.first,
                        repr_path.second
                        );
                    //  Rprintf("Detected origin of expression: %s\n",
                    //  arg_str.c_str());

                    std::string path_root = "";
                    if (const char* runr_cwd = std::getenv("RUNR_CWD")) {
                        path_root = runr_cwd;
                        path_root += "/";
                    }
                    std::string filepath =
                        path_root + function->get_name() + "-" +
                        std::to_string(call->get_id()) + ".dot";
                    ProvenanceGraph::toDot(filepath, call->get_id(), prov);
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
        } else if (event_type == Event::Type::EvalEntry) {
            // NO! eval entry is always called before the ClosureExit 
            // so we will reset the sexp we have just registered in the previous 
            // eval exit
            //eval_sexp.reset();
            //eval_provenance = nullptr;
        } else if (event_type == Event::Type::EvalExit) {
            //  Rprintf("Now in eval entry callback with expression %s and
            //  result %s\n",
            //         deparse(event.get_expression())
            //             .c_str(), deparse(event.get_result()).c_str());

            // the result is the parent of the provenance
            // case of f <- function() parse(text = "1")
            // f()[[1]]

            SEXP el = event.get_expression();

            // it could also be a symbol?
            if (TYPEOF(el) == LANGSXP) {
                SEXP result = event.get_result();

                auto res = addresses.find(result);

                if (res != addresses.end()) {
                    eval_sexp = el;
                    eval_provenance = res->second;
                }
            }
        }
    }

    std::vector<Table*> get_tables() override {
        return {&provenance_table_};
    }

    std::string deparse(const SEXP expr) {
        std::string deparsed = "";
        SEXP res;

        PROTECT(res =
                    Rf_deparse1(expr, FALSE, KEEPINTEGER | KEEPNA | DIGITS17));

        for(int i = 0; i < XLENGTH(res) ; i++) {
            deparsed += CHAR(STRING_ELT(res, i));
            deparsed += "\\n ";
        }
        deparsed.resize(deparsed.size() - 3);
        UNPROTECT(1);
        return deparsed;
    }

  private:
    static int get_provenance_id() {
        return ++provenance_id;
    }

    std::string provenances_from_roots(Provenance* prov) {
        size_t old_size = unique_provenances.size();
        unique_provenances.clear();
        size_t size = std::max(old_size, Provenance::nb_special_functions());
        unique_provenances.reserve(size);
        prov->roots(unique_provenances);

        std::string provenances;
        for (auto prov: unique_provenances) {
            provenances += prov;
            provenances += "; ";
        }
        // Removes the trailing "; "
        provenances.resize(provenances.size() - 2);
        return provenances;
    }
};

#endif