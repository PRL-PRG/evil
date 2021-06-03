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
        std::unordered_map<SEXP, std::tuple<std::string, std::string, int> > addresses;
        // to store multiple provenances
        const static size_t nb_kinds = static_cast<int>(ProvenanceKind::match_call) + 1;
        size_t set_size;
        std::unordered_map<std::string, int> kind;
        std::unordered_set<std::string> args;
        std::unordered_set<int> provenances;
        std::string root_kind;
        inline static const std::unordered_set<std::string> provenance_functions = 
        {"parse", "str2lang", "str2expression", "substitute", "quote", "enquote", "match.call",
        "call", "expresssion", "as.expression"};
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
                
                // Get the return value
                SEXP result = event.get_result();
                
                // Imp2:
                // not only trace the function of interests but also any function resulting
                // in an interesting type.
                // In that case, we have to use an unordered_multimap for the addresses,
                // as wrapping functions will not change the address 
                // e.g. g <- function(t) parse(text = t)

                // if(function->has_identity(Function::Identity::ProvenanceFamily) ||
                //  (result != nullptr && 
                //  (TYPEOF(result) == LANGSXP || TYPEOF(result) == EXPRSXP || TYPEOF(result) == SYMSXP))) {
                
                if(function->has_identity(Function::Identity::ProvenanceFamily)) {
                    // Rprintf("Result is %s, with address %p, with type %s\n", 
                    //     deparse(result, call->get_environment()).c_str(),
                    //     &result,
                    //     CHAR(STRING_ELT(sexp_typeof(result), 0)));

                    std::string provenance =  ProvenanceTable::provenance_to_string(ProvenanceTable::identity_to_provenance(function->get_identity()));
                    //std::string provenance = function->get_name(); // Imp 2

                    std::string arguments = deparse(call->get_expression(), call->get_environment());
                    
                    // For one provenance, one provenance id
                    // i.e. one provenance = one call, not one site
                    auto payload = std::make_tuple(provenance, arguments, get_provenance_id());

                    // Always put the address of the root
                    addresses[result] = payload;

                    // Get address of the value or of the elements if it is a complex expression

                    switch (TYPEOF(result))
                    {
                    case VECSXP:
                    case EXPRSXP: // similar internal representation
                        // Rprintf("Detecting vector or expression\n");
                        for(int i = 0; i < XLENGTH(result); i++) {
                            SEXP el = VECTOR_ELT(result, i);
                            addresses[el] = payload;
                        }
                        break;

                    case LISTSXP:
                    case LANGSXP: {
                        // Rprintf("Detecting language or pairlist\n");
                        for(SEXP cons = result; cons != R_NilValue; cons = CDR(cons)) {
                            SEXP el = CAR(cons);
                            // Also add cons in the addresses?
                            //Rprintf("Adding address %p with SEXP %s\n", el, deparse(el, call->get_environment()));
                            addresses[el] = payload;
                            addresses[cons] = payload;
                        } }
                        break;
                    }

                    // Rprintf("Detected provenance function %s\n", arguments.c_str());
                }

                if(function->has_identity(Function::Identity::EvalFamily)) {
                    // Rprintf("Now in eval! We have %d addresses recorded\n", addresses.size());
                    
                    // for(auto it = addresses.cbegin(); it != addresses.cend(); it++) {
                    //     Rprintf("Address %p with arguments %s and provenance id %d\n", 
                    //     it->first, std::get<1>(it->second).c_str(),
                    //     std::get<2>(it->second));
                    // }
                    // Check if the expression address contains any address saved previously.
                    SEXP expr_promise = event.r_get_argument(Rf_install("expr"), 0);
                    SEXP expr_arg = dyntrace_get_promise_value(expr_promise);
                    // Rprintf("New address %p for %s\n", expr_arg, deparse(expr_arg, call->get_environment()).c_str());

                    assert(call->get_id() != NA_INTEGER);    

                    // multiple provenances
                    clear_sets();

                    bool found = inspect_sexp(expr_arg, 3, true);

                    
                    if(found) {
                         std::string arg_str;
                        for(auto it = args.cbegin(); it != args.cend(); it++) {
                            arg_str += *it + "; ";
                        }
                        provenance_table_.record(call->get_id(),
                            function->get_name(),
                            get_representative(),
                            arg_str.c_str(),
                            provenances.size()); 
                        //  Rprintf("Detected origin of expression: %s\n", arg_str.c_str());

                    } else {
                        // Not found 
                        // Fail safe in the case
                        // the expression was forced before
                        // Indeed, in that case, the previous approach
                        // does not detect 
                        // eval(parse(text = "1"))
                        // so let's look inside the argument directly
                        // NOTE: we still won't detect eval(f(x)) where f has parse in
                        // its body
                        SEXP expr_expr = dyntrace_get_promise_expression(expr_promise);
                        
                        if(TYPEOF(expr_expr) != LANGSXP) {
                            // Not found!
                            return;
                        }
                        
                        SEXP first_call = CAR(expr_expr);
                        if(TYPEOF(first_call) != SYMSXP) {
                            return;
                        }

                        std::string function_name = CHAR(PRINTNAME(first_call));

                        if(provenance_functions.find(function_name) != provenance_functions.end()) {
                            provenance_table_.record(call->get_id(),
                                function->get_name(),
                                function_name,
                                deparse(expr_expr, call->get_environment()) + "; ",
                                1); 
                        }
                    }

                }
            }
            else if(event_type == Event::Type::GcUnmark) {
                // if the SEXP is reclaimed by the GC, we can remove it from
                // the hash table
                // It makes the analysis slightly more correct, by preventing
                // from happening
                // the rare case of an unrelated SEXP allocated at the same address 
                // as a reclaimed address that had been recorded previously

                addresses.erase(event.get_object());
            }
            else if(event_type == Event::Type::BuiltinCallExit) {
                const StackFrame& frame = stack.peek();
                const Call* call = frame.as_call();
                const Function* function = call->get_function();
                Rprintf("Now in builtin %s with expression %s\n", 
                function->get_name().c_str(),
                deparse(call->get_expression(), call->get_environment()).c_str());
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
    bool inspect_sexp(SEXP sexp, int depth, bool root = false) {
        if (sexp == R_NilValue || depth == 0) {
            return false;
        }

        bool found= false;

        // Remember everything
        // indeed, substitute(1) will return a double, not a langsxp
        auto res = addresses.find(sexp);
        if (res != addresses.end()) {
            update_provenances(res->second);
            if(root) {
                // This will actually rarely happen
                // if the root has something inserted in,
                // then it will actually be a new object
                root_kind = std::get<0>(res->second);
                // Rprintf("Detected a root with a known origin: %s\n", root_kind.c_str());
            }
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
        set_size = std::max({set_size, provenances.size(), args.size()});
        kind.clear();
        provenances.clear();
        args.clear();
        root_kind.clear();
        
        kind.reserve(nb_kinds);
        provenances.reserve(set_size);
        args.reserve(set_size);
    }


    // TODO: pick first the provenance of the root of the expression
    // If there is no identified provenance for the root, then the most common provenance
    // ProvenanceKind get_representative() const {
    //     return static_cast<ProvenanceKind>(std::distance(kind.cbegin(), max_element(kind.cbegin(), kind.cend())));
    // }

    std::string get_representative() const {
        if(!root_kind.empty()) {
            return root_kind;
        } else {
            return std::get<0>(*std::max_element(kind.begin(), kind.end(),
                             [](const std::tuple<std::string, int> &p1,
                                const std::tuple<std::string, int> &p2)
                             {
                                 return std::get<1>(p1) < std::get<1>(p2);
                             }));
        } 
    }

    void update_provenances(const std::tuple<std::string, std::string, int>& res) {
        kind[std::get<0>(res)] = 1 +  kind[std::get<0>(res)];
        args.insert(std::get<1>(res));
        provenances.insert(std::get<2>(res));
    }

};

#endif