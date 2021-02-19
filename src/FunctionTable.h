#ifndef EVIL_FUNCTION_TABLE_H
#define EVIL_FUNCTION_TABLE_H

class FunctionTable {
  public:
    FunctionTable() {
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

    void update(SEXP r_closure, SEXP r_name, SEXP r_rho) {
        Function* function = lookup(r_closure);

        if (function->has_name()) {
            return;
        }

        update_name_(function, CHAR(STRING_ELT(r_name, 0)), r_rho);
    }

  private:
    std::unordered_map<SEXP, Function*> table_;

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
