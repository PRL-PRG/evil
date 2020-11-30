#ifndef EVIL_CALL_STATE_H
#define EVIL_CALL_STATE_H

#include "r_utilities.h"

class CallState {
public:
    CallState(int eval_call_id, SEXP r_call, SEXP r_rho, int eval_frame_depth) :
        eval_call_id_(eval_call_id), r_call_(r_call), r_rho_(r_rho), eval_frame_depth_(eval_frame_depth) {
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
        SEXP r_value =
            r_get_argument(r_argument_name, 1);
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
            if(r_result != NA_STRING) {
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

private:
    int eval_call_id_;
    SEXP r_call_;
    SEXP r_rho_;
    int eval_frame_depth_;
};

#endif /* EVIL_CALL_STATE_H */
