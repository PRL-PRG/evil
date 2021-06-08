#ifndef EVIL_FUNCTION_H
#define EVIL_FUNCTION_H

#include <string>
#include <climits>
#include "Rdyntrace.h"

class Function {
  public:
    enum class Identity {
        Eval = 1,
        EvalQ = 2,
        EvalParent = 4,
        Local = 8,
        Library = 16,
        Require = 32,
        AttachNamespace = 64,
        LoadNamespace = 128,
        RequireNamespace = 256,
        UnloadNamespace = 512,
        LazyLoad = 1024,
        LazyLoadDbExec = 2048,
        NewEnv = 4096,
        List2Env = 8192,
        Parse = 16384, 
        Str2lang = 1 << 15, 
        Str2expression = 1 << 16,
        Substitute = 1 << 17,
        Quote = 1 << 18,
        Enquote = 1 << 19,
        Match_call = 1 << 20,
        Call = 1 << 21,
        AsCall = 1 << 22,
        Expression = 1 << 23,
        AsExpression = 1 << 24,
        AsName = 1 << 25,
        AsSymbol = 1 << 26,
        AsFormula = 1 << 27,
        Tilde = 1 << 28,
        Other = 1 << 29,
        EvalFamily = Eval | EvalQ | EvalParent | Local,
        PackageFamily = Library | Require | AttachNamespace | LoadNamespace |
                        RequireNamespace | UnloadNamespace | LazyLoad |
                        LazyLoadDbExec,
        EnvironmentFamily = NewEnv | List2Env,
        ProvenanceFamily = Parse | Str2lang | Str2expression | Substitute | Quote | 
            Enquote | Match_call | Call | AsCall | Expression | AsExpression | AsName |
             AsSymbol | Tilde | AsFormula,
        Any = INT_MAX
    };

    explicit Function(SEXP r_op)
        : r_op_(r_op), identity_(Identity::Other), ref_(1) {
        type_ = TYPEOF(r_op);

        if (type_ == BUILTINSXP || type_ == SPECIALSXP) {
            package_name_ = "base";
            name_ = dyntrace_get_c_function_name(r_op);
        } else {
            SEXP r_lexenv = CLOENV(r_op);

            if (r_lexenv == R_GlobalEnv) {
                package_name_ = "global";
            }

            else if (r_lexenv == R_BaseEnv || r_lexenv == R_BaseNamespace) {
                package_name_ = "base";
            }

            else if (R_IsPackageEnv(r_lexenv)) {
                package_name_ = CHAR(STRING_ELT(R_PackageEnvName(r_lexenv), 0));

            }

            else if (R_IsNamespaceEnv(r_lexenv)) {
                package_name_ =
                    CHAR(STRING_ELT(R_NamespaceEnvSpec(r_lexenv), 0));
            }
        }
    }

    SEXP get_op() {
        return r_op_;
    }

    SEXPTYPE get_type() const {
        return type_;
    }

    bool is_closure() const {
        return type_ == CLOSXP;
    }

    bool is_special() const {
        return type_ == SPECIALSXP;
    }

    bool is_builtin() const {
        return type_ == BUILTINSXP;
    }

    bool has_name() const {
        return !name_.empty();
    }

    const std::string& get_name() const {
        return name_;
    }

    void set_name(const char* name) {
        name_ = name;
    }

    bool has_package_name() const {
        return !package_name_.empty();
    }

    const std::string& get_package_name() const {
        return package_name_;
    }

    std::string get_qualified_name() const {
        std::string qualified_name("");

        if (has_package_name()) {
            qualified_name.append(get_package_name());
            qualified_name.append("::");
        }

        std::string name("<unknown>");

        if (has_name()) {
            name = get_name();
        }

        qualified_name.append(name);

        return qualified_name;
    }

    void set_identity(Identity identity) {
        identity_ = identity;
    }

    Function::Identity get_identity() const {
        return identity_;
    }

    bool has_identity(Function::Identity identity) const {
        if (identity == identity_)
            return true;

        return (int) (identity_) & (int) (identity);
    }

    int get_parent_eval_id() const {
        return parent_eval_id_;
    }

    void set_parent_eval_id(int eval_id) {
        parent_eval_id_ = eval_id;
    }

    static void inc_ref(Function* function);

    static void dec_ref(Function* function);

  private:
    int ref_;
    SEXP r_op_;
    SEXPTYPE type_;
    std::string name_;
    std::string package_name_;
    Identity identity_;
    int parent_eval_id_;

    ~Function() {
    }
};

#endif /* EVIL_FUNCTION_H */
