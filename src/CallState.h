#ifndef EVIL_CALL_STATE_H
#define EVIL_CALL_STATE_H

#include "r_utilities.h"
#include "Event.h"

class CallState {
  public:
    Event get_event() const {
        return event_;
    }

    int get_eval_call_id() {
        return eval_call_id_;
    }

    SEXP get_call() {
        return r_call_;
    }

    SEXP get_rho() {
        return r_rho_;
    }

    SEXP get_variable() {
        return r_variable_;
    }

    SEXP get_value() {
        return r_value_;
    }

    int get_eval_frame_depth() {
        return eval_frame_depth_;
    }

    int get_current_frame_depth() {
        return dyntrace_get_frame_depth();
    }

    SEXP r_get_argument(SEXP r_argument_name, int evaluate) {
        SEXP r_value = Rf_findVarInFrame(get_rho(), r_argument_name);

        if (r_value == R_MissingArg || TYPEOF(r_value) != PROMSXP) {
            return r_value;
        } else if (evaluate) {
            return Rf_eval(r_value, get_rho());
        } else {
            return r_value;
        }
    }

    int get_integer_argument(SEXP r_argument_name) {
        SEXP r_value = r_get_argument(r_argument_name, 1);
        int result = NA_INTEGER;

        if (TYPEOF(r_value) == REALSXP) {
            double value = REAL(r_value)[0];
            result = value == NA_REAL ? NA_INTEGER : (int) (value);
        } else if (TYPEOF(r_value) == INTSXP) {
            result = INTEGER(r_value)[0];
        }

        return result;
    }

    int get_logical_argument(SEXP r_argument_name) {
        SEXP r_value = r_get_argument(r_argument_name, 1);
        int result = NA_LOGICAL;

        if (TYPEOF(r_value) == LGLSXP) {
            result = LOGICAL(r_value)[0];
        } else if (TYPEOF(r_value) == INTSXP) {
            result = INTEGER(r_value)[0];
        }

        return result;
    }

    std::string get_character_argument(SEXP r_argument_name) {
        SEXP r_value = r_get_argument(r_argument_name, 1);
        std::string result = MissingStringValue;

        if (TYPEOF(r_value) == STRSXP) {
            SEXP r_result = STRING_ELT(r_value, 0);
            if (r_result != NA_STRING) {
                result = std::string(CHAR(r_result));
            }
        }

        return result;
    }

    int is_call_to(const char* function_name) {
        SEXP r_function_name = CAR(get_call());
        int library =
            TYPEOF(r_function_name) == SYMSXP &&
            (strcmp(function_name, CHAR(PRINTNAME(r_function_name))) == 0);
        return library;
    }

    static CallState closure_call_entry(int call_id,
                                        SEXP r_call,
                                        SEXP r_rho,
                                        int eval_frame_depth);

    static CallState
    variable_definition(int call_id, SEXP r_variable, SEXP r_value, SEXP r_rho);

    static CallState
    variable_assignment(int call_id, SEXP r_variable, SEXP r_value, SEXP r_rho);

    static CallState variable_removal(int call_id, SEXP r_variable, SEXP r_rho);

    static CallState
    variable_lookup(int call_id, SEXP r_variable, SEXP r_value, SEXP r_rho);

  private:
    Event event_;
    int eval_call_id_;
    SEXP r_call_;
    SEXP r_rho_;
    int eval_frame_depth_;
    SEXP r_variable_;
    SEXP r_value_;

    CallState(Event event, int eval_call_id)
        : event_(event)
        , eval_call_id_(eval_call_id)
        , r_call_(NULL)
        , r_rho_(NULL)
        , eval_frame_depth_(-1)
        , r_variable_(NULL)
        , r_value_(NULL) {
    }

    CallState(Event event,
              int eval_call_id,
              SEXP r_call,
              SEXP r_rho,
              int eval_frame_depth)
        : CallState(event, eval_call_id) {
        r_call_ = r_call;
        r_rho_ = r_rho;
        eval_frame_depth_ = eval_frame_depth;
    }

    CallState(Event event,
              int eval_call_id,
              SEXP r_variable,
              SEXP r_value,
              SEXP r_rho)
        : CallState(event, eval_call_id) {
        r_variable_ = r_variable;
        r_value_ = r_value;
        r_rho_ = r_rho;
    }

    CallState(Event event, int eval_call_id, SEXP r_variable, SEXP r_rho)
        : CallState(event, eval_call_id) {
        r_variable_ = r_variable;
        r_rho_ = r_rho;
    }
};

#endif /* EVIL_CALL_STATE_H */
