#ifndef EVIL_FUNCTION_TABLE_H
#define EVIL_FUNCTION_TABLE_H

#include <R.h>
#include <Rinternals.h>

class FunctionTable {
  public:
    FunctionTable() {
        for (SEXP r_rho = R_GlobalEnv; r_rho != R_EmptyEnv;
             r_rho = ENCLOS(r_rho)) {
            SEXP r_names = R_lsInternal(r_rho, TRUE);
            for (int i = 0; i < Rf_length(r_names); ++i) {
                const char* name = CHAR(STRING_ELT(r_names, i));
                SEXP r_obj = Rf_findVarInFrame(r_rho, Rf_install(name));
                update(r_obj, name, r_rho);
            }
        }
    }

    ~FunctionTable() {
        for (auto& it: table_) {
            delete it.second;
        }
    }

    void insert(SEXP r_closure) {
        Function* function = new Function(r_closure);

        auto result = table_.insert({r_closure, function});

        if (!result.second) {
            delete result.first->second;
            result.first->second = function;
        }
    }

    void remove(SEXP r_closure) {
        auto result = table_.find(r_closure);

        if (result != table_.end()) {
            Function* function = result->second;
            table_.erase(result);
            delete function;
        }
    }

    Function* lookup(SEXP r_closure) {
        return get_or_create_(r_closure);
    }

    Function* lookup(SEXP r_closure, SEXP r_call, SEXP r_rho) {
        Function* function = lookup(r_closure);

        if (function->has_name()) {
            return function;
        }

        SEXP r_call_name = CAR(r_call);

        if (TYPEOF(r_call_name) != SYMSXP) {
            return function;
        }

        SEXP obj = Rf_findVarInFrame(r_rho, r_call_name);

        if (obj == R_UnboundValue) {
            return function;
        }

        if (obj == r_closure) {
            update_name_(function, CHAR(PRINTNAME(r_call_name)), r_rho);
        } else if (TYPEOF(obj) == PROMSXP) {
            if (dyntrace_get_promise_value(obj) == r_closure ||
                dyntrace_get_promise_expression(obj) == r_closure) {
                update_name_(function, CHAR(PRINTNAME(r_call_name)), r_rho);
            }
        }

        return function;
    }

    void update(SEXP r_value, const char* name, SEXP r_rho) {
        SEXP r_closure = unwrap_function_(r_value);

        if (TYPEOF(r_closure) != CLOSXP) {
            return;
        }

        Function* function = lookup(r_closure);

        if (function->has_name()) {
            return;
        }

        update_name_(function, name, r_rho);
    }

  private:
    std::unordered_map<SEXP, Function*> table_;

    SEXP unwrap_function_(SEXP r_value) {
        SEXP r_closure = R_NilValue;

        switch (TYPEOF(r_value)) {
        case CLOSXP:
            r_closure = r_value;
            break;
        case PROMSXP:
            r_closure = dyntrace_get_promise_value(r_value);
            if (r_closure == R_UnboundValue || TYPEOF(r_closure) != CLOSXP) {
                r_closure = dyntrace_get_promise_expression(r_value);
                if (r_closure == R_UnboundValue ||
                    TYPEOF(r_closure) != CLOSXP) {
                    r_closure = R_NilValue;
                }
            }
            break;
        default:
            break;
        }

        return r_closure;
    }

    Function* get_or_create_(SEXP r_closure) {
        auto result = table_.find(r_closure);

        if (result != table_.end()) {
            return result->second;
        } else {
            Function* function = new Function(r_closure);
            auto result = table_.insert({r_closure, function});
            return function;
        }
    }

    Function* update_name_(Function* function, const char* name, SEXP r_rho) {
        SEXP r_lexenv = CLOENV(function->get_op());

        /* function has a name in its lexical env */
        if (r_lexenv == r_rho) {
            function->set_name(name);
        }

        return function;
    }
};

#endif /* EVIL_FUNCTION_TABLE_H */
