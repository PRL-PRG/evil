#ifndef EVIL_FUNCTION_H
#define EVIL_FUNCTION_H

#include <string>

class Function {
  public:
    explicit Function(SEXP r_op): r_op_(r_op) {
        type_ = TYPEOF(r_op);

        if (type_ == BUILTINSXP || type_ == SPECIALSXP) {
            package_name_ = "base";
            name_ = dyntrace_get_c_function_name(r_op);
        } else {
            SEXP r_lexenv = CLOENV(r_op);

            SEXP r_package_name = R_PackageEnvName(r_lexenv);

            if (r_package_name != R_NilValue) {
                package_name_ = CHAR(STRING_ELT(r_package_name, 0));
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

    const std::string& get_package_name() const {
        return package_name_;
    }

  private:
    SEXP r_op_;
    SEXPTYPE type_;
    std::string name_;
    std::string package_name_;
};

#endif /* EVIL_FUNCTION_H */
