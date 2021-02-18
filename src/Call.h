#ifndef EVIL_CALL_H
#define EVIL_CALL_H

#include "Function.h"

class Function;

typedef std::vector<int> force_order_t;

class Call {
  public:
    Call(Function* function, SEXP r_call, SEXP r_args, SEXP r_rho)
        : r_call_(r_call)
        , r_args_(r_args)
        , r_rho_(r_rho)
        , interrupted_(false)
        , function_(function) {
    }

    Function* get_function() {
        return function_;
    }

    const Function* get_function() const {
        return function_;
    }

    SEXP get_expression() {
        return r_call_;
    }

    SEXP get_arguments() {
        return r_args_;
    }

    SEXP get_environment() {
        return r_rho_;
    }

    void set_interrupted() {
        interrupted_ = true;
    }

    bool is_interrupted() const {
        return interrupted_;
    }

  private:
    Function* function_;
    SEXP r_call_;
    SEXP r_args_;
    SEXP r_rho_;
    bool interrupted_;
};

#endif /* EVIL_CALL_H */
