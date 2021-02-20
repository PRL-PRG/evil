#ifndef EVIL_FUNCTION_H
#define EVIL_FUNCTION_H

#include <string>

class Function {
  public:
    enum class Type {
        Eval = 1,
        EvalQ = 2,
        EvalParent = 4,
        Local = 8,
        EvalFamily = 1 | 2 | 4 | 8,
        Other = 16,
        Any = INT_MAX
    };

    explicit Function(SEXP r_op): r_op_(r_op), type_(Type::Other) {
        optype_ = TYPEOF(r_op);

        if (optype_ == BUILTINSXP || optype_ == SPECIALSXP) {
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

    SEXPTYPE get_optype() const {
        return optype_;
    }

    bool is_closure() const {
        return optype_ == CLOSXP;
    }

    bool is_special() const {
        return optype_ == SPECIALSXP;
    }

    bool is_builtin() const {
        return optype_ == BUILTINSXP;
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

    bool is_eval_family() const {
        return (int) (type_) | (int) (Type::EvalFamily);
    }

    bool is_eval() const {
        return type_ == Type::Eval;
    }

    bool is_eval_parent() const {
        return type_ == Type::EvalParent;
    }

    bool is_evalq() const {
        return type_ == Type::EvalQ;
    }

    bool is_local() const {
        return type_ == Type::Local;
    }

    bool is_other() const {
        return type_ == Type::Other;
    }

    bool is_any() const {
        return true;
    }

    void set_type(Type type) {
        type_ = type;
    }

    Function::Type get_type() const {
        return type_;
    }

    bool has_type(Function::Type type) const {
        switch (type) {
        case Type::Any:
            return is_any();
        case Type::Other:
            return is_other();
        case Type::Eval:
            return is_eval();
        case Type::EvalFamily:
            return is_eval_family();
        case Type::EvalQ:
            return is_evalq();
        case Type::EvalParent:
            return is_eval_parent();
        case Type::Local:
            return is_local();
        }

        Rf_error("unhandled type");
    }

  private:
    SEXP r_op_;
    SEXPTYPE optype_;
    std::string name_;
    std::string package_name_;
    Type type_;
};

#endif /* EVIL_FUNCTION_H */
