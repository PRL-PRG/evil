#ifndef EVIL_FUNCTION_TABLE_H
#define EVIL_FUNCTION_TABLE_H

#include <R.h>
#include <Rinternals.h>
#include "Function.h"
#include <unordered_set>
#include <unordered_map>

class FunctionTable {
  public:
    FunctionTable() {
        initialize_identity_();
    }

    void handle_packages() {
        SEXP r_package_names = R_lsInternal(R_NamespaceRegistry, TRUE);
        for (int i = 0; i < Rf_length(r_package_names); ++i) {
            const char* name = CHAR(STRING_ELT(r_package_names, i));
            SEXP r_rho =
                Rf_findVarInFrame(R_NamespaceRegistry, Rf_install(name));

            if (TYPEOF(r_rho) != ENVSXP) {
                continue;
            }

            SEXP r_namespace = infer_namespace_(r_rho);

            std::string package_name = get_package_name_(r_namespace);

            if (handled_packages_.find(package_name) !=
                handled_packages_.end()) {
                continue;
            }

            // Rprintf("FunctionTable: handling package '%s'\n",
            //         package_name.c_str());

            SEXP r_names = R_lsInternal(r_namespace, TRUE);

            for (int i = 0; i < Rf_length(r_names); ++i) {
                const char* name = CHAR(STRING_ELT(r_names, i));
                SEXP r_obj = Rf_findVarInFrame(r_namespace, Rf_install(name));

                update(r_obj, name, r_namespace);
            }

            handled_packages_.insert(package_name);
        }
    }

    void set_function_identity_(SEXP r_rho,
                                const char* name,
                                Function::Identity identity) {
        SEXP r_fun =
            unwrap_function_(Rf_findVarInFrame(r_rho, Rf_install(name)));

        if (TYPEOF(r_fun) != CLOSXP) {
            Rf_error(
                "set_function_identity_: expected CLOSXP after unwrapping %s", name);
        }

        Function* fun = lookup(r_fun);
        fun->set_identity(identity);
    }

    ~FunctionTable() {
        for (auto& it: table_) {
            Function::dec_ref(it.second);
        }
    }

    Function* insert(SEXP r_closure) {
        Function* function = new Function(r_closure);

        auto result = table_.insert({r_closure, function});

        if (!result.second) {
            Function::dec_ref(result.first->second);
            result.first->second = function;
        }

        return function;
    }

    void remove(SEXP r_closure) {
        Function::Identity identity = Function::Identity::Other;

        auto result = table_.find(r_closure);

        if (result != table_.end()) {
            Function* function = result->second;

            identity = function->get_identity();

            table_.erase(result);
            Function::dec_ref(function);
        }

        if (identity != Function::Identity::Other) {
            initialize_identity_();
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
    std::unordered_set<std::string> handled_packages_;

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

    std::string get_package_name_(SEXP r_package_env) {
        if (r_package_env == R_GlobalEnv) {
            return "global";
        }

        else if (r_package_env == R_BaseEnv ||
                 r_package_env == R_BaseNamespace) {
            return "base";
        }

        else if (R_IsPackageEnv(r_package_env)) {
            return CHAR(STRING_ELT(R_PackageEnvName(r_package_env), 0)) +
                   strlen("package:");

        }

        else if (R_IsNamespaceEnv(r_package_env)) {
            return CHAR(STRING_ELT(R_NamespaceEnvSpec(r_package_env), 0));
        }

        else {
            return "<unknown>";
            Rprintf("unable to get name of package environment %p\n",
                    r_package_env);
        }
    }

    void initialize_identity_() {
        set_function_identity_(
            R_BaseNamespace, "eval", Function::Identity::Eval);
        set_function_identity_(
            R_BaseNamespace, "evalq", Function::Identity::EvalQ);
        set_function_identity_(
            R_BaseNamespace, "eval.parent", Function::Identity::EvalParent);
        set_function_identity_(
            R_BaseNamespace, "local", Function::Identity::Local);
        set_function_identity_(
            R_BaseNamespace, "library", Function::Identity::Library);
        set_function_identity_(
            R_BaseNamespace, "require", Function::Identity::Require);
        set_function_identity_(
            R_BaseNamespace, "lazyLoad", Function::Identity::LazyLoad);
        set_function_identity_(R_BaseNamespace,
                               "lazyLoadDBexec",
                               Function::Identity::LazyLoadDbExec);
        set_function_identity_(R_BaseNamespace,
                               "attachNamespace",
                               Function::Identity::AttachNamespace);
        set_function_identity_(R_BaseNamespace,
                               "loadNamespace",
                               Function::Identity::LoadNamespace);
        set_function_identity_(R_BaseNamespace,
                               "requireNamespace",
                               Function::Identity::RequireNamespace);
        set_function_identity_(R_BaseNamespace,
                               "unloadNamespace",
                               Function::Identity::UnloadNamespace);
//        set_function_identity_(R_BaseNamespace, "new.env", Function::Identity::NewEnv);
        set_function_identity_(R_BaseNamespace,
                                "parse",
                                Function::Identity::Parse);
        set_function_identity_(
            R_BaseNamespace, "str2lang", Function::Identity::Str2lang);
        set_function_identity_(
            R_BaseNamespace, "str2expression", Function::Identity::Str2expression);

        // Those are 
        // set_function_identity_(
        //     R_BaseEnv, "substitute", Function::Identity::Substitute);
        // set_function_identity_(
        //     R_BaseEnv, "quote", Function::Identity::Quote);
        set_function_identity_(
            R_BaseNamespace, "enquote", Function::Identity::Enquote);
        set_function_identity_(
            R_BaseNamespace, "match.call", Function::Identity::Match_call);

    }
};

#endif /* EVIL_FUNCTION_TABLE_H */
