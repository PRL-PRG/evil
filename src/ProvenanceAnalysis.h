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
    ProvenanceTable provenance_table_;
    ProvenanceGraph provenance_graph_;
    std::unordered_map<SEXP, Provenance*> > addresses; // or unordered_multimap?
    // to store multiple provenances
    size_t set_size;
    std::unordered_set<Provenance*> provenances;
    inline static const std::unordered_set<std::string> provenance_functions = {
        "parse",
        "str2lang",
        "str2expression",
        "substitute",
        "quote",
        "enquote",
        "match.call",
        "call",
        "expresssion",
        "as.expression"};

  public:
    ProvenanceAnalysis(): Analysis(), set_size(10), provenances(set_size) {
    }

    void analyze(TracerState& tracer_state, Event& event) override {
        Event::Type event_type = event.get_type();
        Stack& stack = tracer_state.get_stack();

        if (event_type == Event::Type::ClosureCallExit ||
            event_type == Event::Type::SpecialCallExit) {
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            const Function* function = call->get_function();

            // Get the return value
            SEXP result = event.get_result();

            if (function->has_identity(Function::Identity::ProvenanceFamily) ||
                (result != nullptr &&
                 (TYPEOF(result) == LANGSXP || TYPEOF(result) == EXPRSXP ||
                  TYPEOF(result) == SYMSXP))) {
                // Rprintf("Result is %s, with address %p, with type %s\n",
                //     deparse(result, call->get_environment()).c_str(),
                //     &result,
                //     CHAR(STRING_ELT(sexp_typeof(result), 0)));

                std::string function_name = function->get_name();

                std::string full_call =
                    deparse(call->get_expression(), call->get_environment());

                Provenance* payload = provenance_graph_.add_node(
                    result, function_name, full_call, get_provenance_id());

                // Always put the address of the root
                addresses[result] = payload;

                // Get address of the value or of the elements if it is a
                // complex expression

                switch (TYPEOF(result)) {
                case VECSXP:
                case EXPRSXP: // similar internal representation
                    // Rprintf("Detecting vector or expression\n");
                    for (int i = 0; i < XLENGTH(result); i++) {
                        SEXP el = VECTOR_ELT(result, i);
                        addresses[el] = payload;
                    }
                    break;

                case LISTSXP:
                case LANGSXP: {
                    // Rprintf("Detecting language or pairlist\n");
                    for (SEXP cons = result; cons != R_NilValue;
                         cons = CDR(cons)) {
                        SEXP el = CAR(cons);
                        // Also add cons in the addresses?
                        // Rprintf("Adding address %p with SEXP %s\n", el,
                        // deparse(el, call->get_environment()));
                        addresses[el] = payload;
                        addresses[cons] = payload;
                    }
                } break;
                }

                // Rprintf("Detected provenance function %s\n",
                // arguments.c_str());

                // Now, also check if the argument of the expression had also
                // been recorded in the table
                SEXP args = CDR(call->get_expression());
                for (SEXP const = args; cons != R_NilValue; cons = CDR(cons)) {
                    SEXP el = CAR(cons);
                    SEXP expr_promise = event.r_get_argument(el, 0);
                    SEXP expr_arg = dyntrace_get_promise_value(expr_promise);
                    // Here, we need to traverse each of the argument and check
                    // if they are in the hash table if yes, we can add the
                    // provenance(s) of this argument as parents of the current
                    // provenance
                    clear_sets();

                    bool found = inspect_sexp(expr_arg, 3);

                    if (found) {
                        for (auto prov: provenances) {
                            payload->add_parent(prov);
                        }
                    }
                }
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

                // multiple provenances
                clear_sets();

                bool found = inspect_sexp(expr_arg, 3);

                if (found) {
                    std::string arg_str;
                    for (auto it = args.cbegin(); it != args.cend(); it++) {
                        arg_str += *it + "; ";
                    }
                    provenance_table_.record(call->get_id(),
                                             function->get_name(),
                                             get_representative(),
                                             arg_str.c_str(),
                                             provenances.size());
                    //  Rprintf("Detected origin of expression: %s\n",
                    //  arg_str.c_str());

                } else {
                    // Not found
                    // Fail safe in the case
                    // the expression was forced before
                    // Indeed, in that case, the previous approach
                    // does not detect
                    // eval(parse(text = "1"))
                    // so let's look inside the argument directly
                    // NOTE: we still won't detect eval(f(x)) where f has parse
                    // in its body
                    SEXP expr_expr =
                        dyntrace_get_promise_expression(expr_promise);

                    if (TYPEOF(expr_expr) != LANGSXP) {
                        // Not found!
                        return;
                    }

                    SEXP first_call = CAR(expr_expr);
                    if (TYPEOF(first_call) != SYMSXP) {
                        return;
                    }

                    std::string function_name = CHAR(PRINTNAME(first_call));

                    if (provenance_functions.find(function_name) !=
                        provenance_functions.end()) {
                        provenance_table_.record(
                            call->get_id(),
                            function->get_name(),
                            function_name,
                            deparse(expr_expr, call->get_environment()) + "; ",
                            1);
                    }
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
                    deparse(call->get_expression(), call->get_environment())
                        .c_str());
        }
    }

    std::vector<Table*> get_tables() override {
        return {&provenance_table_};
    }

    std::string deparse(const SEXP expr, const SEXP env) {
        std::string deparsed = "ERROR";
        SEXP res;

        PROTECT(res =
                    Rf_deparse1(expr, FALSE, KEEPINTEGER | KEEPNA | DIGITS17));
        deparsed = CHAR(STRING_ELT(res, 0));
        UNPROTECT(1);
        return deparsed;
    }

  private:

    // We probably do not have to inspect the full content of the SEXP if we use the dataflow model
    // Indeed, the dataflow model will record everything that builds the expression
    bool inspect_sexp(SEXP sexp, int depth) {
        if (sexp == R_NilValue || depth == 0) {
            return false;
        }

        bool found = false;

        // Remember everything
        // indeed, substitute(1) will return a double, not a langsxp
        auto res = addresses.find(sexp);
        if (res != addresses.end()) {
            update_provenances(res->second);
            found = true;
        }

        // Now we look inside the expression
        switch (TYPEOF(sexp)) {
        case VECSXP:
        case EXPRSXP: {
            for (int i = 0; i < XLENGTH(sexp); i++) {
                SEXP el = VECTOR_ELT(sexp, i);
                found |= inspect_sexp(el, depth - 1);
            }
        } break;

        case LISTSXP:
        case LANGSXP: {
            found |= inspect_sexp(CDR(sexp), depth);
            SEXP el = CAR(sexp);
            found |= inspect_sexp(el, depth - 1);
        } break;

        default: // do nothing
            break;
        }
        return found;
    }

    static int get_provenance_id() {
        return ++provenance_id;
    }

    void clear_sets() {
        set_size = std::max({set_size, provenances.size()});
        provenances.clear();

        provenances.reserve(set_size);
    }

    std::string get_representative() const {
        if (!root_kind.empty()) {
            return root_kind;
        } else {
            return std::get<0>(
                *std::max_element(kind.begin(),
                                  kind.end(),
                                  [](const std::tuple<std::string, int>& p1,
                                     const std::tuple<std::string, int>& p2) {
                                      return std::get<1>(p1) < std::get<1>(p2);
                                  }));
        }
    }

    void update_provenances(Provenance* prov) {
        provenances.insert(prov);
    }
};

#endif