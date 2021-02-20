#ifndef EVIL_FUNCTION_TABLE_H
#define EVIL_FUNCTION_TABLE_H

#include <R.h>
#include <Rinternals.h>

class FunctionTable {
  public:
    FunctionTable() {
    }

    void initialize() {
        for (SEXP r_rho = R_GlobalEnv; r_rho != R_EmptyEnv;
             r_rho = ENCLOS(r_rho)) {
            SEXP r_namespace = infer_namespace_(r_rho);

            SEXP r_names = R_lsInternal(r_namespace, TRUE);

            for (int i = 0; i < Rf_length(r_names); ++i) {
                const char* name = CHAR(STRING_ELT(r_names, i));
                SEXP r_obj = Rf_findVarInFrame(r_namespace, Rf_install(name));

                update(r_obj, name, r_namespace);
            }
        }

        set_function_type_("eval", Function::Type::Eval);
        set_function_type_("evalq", Function::Type::EvalQ);
        set_function_type_("eval.parent", Function::Type::EvalParent);
        set_function_type_("local", Function::Type::Local);
    }

    void set_function_type_(const char* name, Function::Type type) {
        SEXP r_fun = unwrap_function_(
            Rf_findVarInFrame(R_BaseNamespace, Rf_install(name)));

        if (TYPEOF(r_fun) != CLOSXP) {
            Rf_error("set_function_type_: expected CLOSXP after unwrapping");
        }

        Function* fun = lookup(r_fun);
        fun->set_type(type);
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

    SEXP infer_namespace_(SEXP r_package) {
        if (r_package == R_BaseEnv) {
            return R_BaseNamespace;
        }

        else if (r_package == R_GlobalEnv) {
            return R_GlobalEnv;
        }

        else if (R_IsNamespaceEnv(r_package)) {
            return r_package;
        }

        else if (R_IsPackageEnv(r_package)) {
            const char* package_name =
                CHAR(STRING_ELT(R_PackageEnvName(r_package), 0));
            return Rf_findVarInFrame(
                R_NamespaceRegistry,
                Rf_install(package_name + strlen("package:")));
        }

        else {
            return r_package;
        }
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
        /* NOTE: A function's lexical environment is a namespace. If r_rho is a
         * package environment, we retrieve the corresponding namespace by
         * querying the namespace registry with the package name (without the
         * "package:" prefix)*/
        if (R_IsPackageEnv(r_rho)) {
            r_rho = infer_namespace_(r_rho);
        }

        SEXP r_lexenv = CLOENV(function->get_op());

        /* function has a name in its lexical env */
        if (r_lexenv == r_rho) {
            function->set_name(name);
        }

        return function;
    }
};

#endif /* EVIL_FUNCTION_TABLE_H */
