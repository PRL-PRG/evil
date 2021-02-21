#ifndef EVIL_CALL_H
#define EVIL_CALL_H

#include <R.h>
#include <Rinternals.h>
#include "Function.h"

class Call {
  public:
    enum class Status { Active, Inactive, Interrupted };

    Call(Function* function, SEXP r_call, SEXP r_args, SEXP r_rho, int depth)
        : function_(function)
        , status_(Status::Active)
        , r_call_(r_call)
        , r_args_(r_args)
        , r_rho_(r_rho)
        , depth_(depth)
        , interrupted_(false)
        , id_(NA_INTEGER)
        , eval_env_(R_NilValue)
        , interp_eval_count_(0)
        , ref_(1) {
        Function::inc_ref(function_);
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

    int get_depth() {
        return depth_;
    }

    void set_status(Status status) {
        status_ = status;
    }

    bool is_active() const {
        return status_ == Status::Active;
    }

    bool is_inactive() const {
        return status_ == Status::Inactive;
    }

    bool is_interrupted() const {
        return status_ == Status::Interrupted;
    }

    int get_id() const {
        return id_;
    }

    void set_id(int id) {
        id_ = id;
    }

    SEXP get_eval_environment() {
        return eval_env_;
    }

    void set_eval_environment(SEXP eval_env) {
        eval_env_ = eval_env;
    }

    int get_interp_eval_count() const {
        return interp_eval_count_;
    }

    void increment_interp_eval_count() {
        ++interp_eval_count_;
    }

    static void inc_ref(Call* call);

    static void dec_ref(Call* call);

  private:
    Function* function_;
    Status status_;
    SEXP r_call_;
    SEXP r_args_;
    SEXP r_rho_;
    int depth_;
    bool interrupted_;
    int id_;
    SEXP eval_env_;
    int interp_eval_count_;
    int ref_;

    ~Call() {
        Function::dec_ref(function_);
    }
};

#endif /* EVIL_CALL_H */
