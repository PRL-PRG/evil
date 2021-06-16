#ifndef EVIL_EVENT_H
#define EVIL_EVENT_H

#include "r_utilities.h"

#include <string>

class Event {
  public:
    enum class Type {
        EvalEntry,
        ClosureCallEntry,
        ClosureCallExit,
        SpecialCallEntry,
        SpecialCallExit,
        BuiltinCallEntry,
        BuiltinCallExit,
        VariableDefinition,
        VariableAssignment,
        VariableRemoval,
        VariableLookup,
        ContextEntry,
        ContextExit,
        GcAllocation,
        GcUnmark
    };

    Type get_type() const {
        return type_;
    }

    SEXP get_call() {
        return r_call_;
    }

    SEXP get_op() {
        return r_op_;
    }

    SEXP get_args() {
        return r_args_;
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

    SEXP get_result() {
        return r_result_;
    }

    void* get_call_context() {
        return call_context_;
    }

    SEXP get_object() {
        return r_object_;
    }

    SEXP get_expression() const {
        return r_expression_;
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

    std::string get_short_name() const;

    static Event eval_entry(SEXP r_expression, SEXP r_rho);

    static Event
    closure_call_entry(SEXP r_call, SEXP r_op, SEXP r_args, SEXP r_rho);

    static Event closure_call_exit(SEXP r_call,
                                   SEXP r_op,
                                   SEXP r_rho,
                                   SEXP r_args,
                                   SEXP r_result);

    static Event
    special_call_entry(SEXP r_call, SEXP r_op, SEXP r_args, SEXP r_rho);

    static Event special_call_exit(SEXP r_call,
                                   SEXP r_op,
                                   SEXP r_rho,
                                   SEXP r_args,
                                   SEXP r_results);

    static Event
    builtin_call_entry(SEXP r_call, SEXP r_op, SEXP r_args, SEXP r_rho);

    static Event builtin_call_exit(SEXP r_call,
                                   SEXP r_op,
                                   SEXP r_rho,
                                   SEXP r_args,
                                   SEXP r_results);


    static Event variable_definition(SEXP r_variable, SEXP r_value, SEXP r_rho);

    static Event variable_assignment(SEXP r_variable, SEXP r_value, SEXP r_rho);

    static Event variable_removal(SEXP r_variable, SEXP r_rho);

    static Event variable_lookup(SEXP r_variable, SEXP r_value, SEXP r_rho);

    static Event context_entry(void* call_context);

    static Event context_exit(void* call_context);

    static Event gc_allocation(SEXP r_object);

    static Event gc_unmark(SEXP r_object);

  private:
    Event(Event::Type type)
        : type_(type)
        , r_call_(NULL)
        , r_op_(NULL)
        , r_args_(NULL)
        , r_rho_(NULL)
        , r_variable_(NULL)
        , r_value_(NULL)
        , r_result_(NULL)
        , call_context_(NULL)
        , r_object_(NULL)
        , r_expression_(NULL) {
    }

    Event& set_call(SEXP r_call) {
        r_call_ = r_call;
        return *this;
    }

    Event& set_op(SEXP r_op) {
        r_op_ = r_op;
        return *this;
    }

    Event& set_args(SEXP r_args) {
        r_args_ = r_args;
        return *this;
    }

    Event& set_rho(SEXP r_rho) {
        r_rho_ = r_rho;
        return *this;
    }

    Event& set_variable(SEXP r_variable) {
        r_variable_ = r_variable;
        return *this;
    }

    Event& set_value(SEXP r_value) {
        r_value_ = r_value;
        return *this;
    }

    Event& set_result(SEXP r_result) {
        r_result_ = r_result;
        return *this;
    }

    Event& set_call_context(void* call_context) {
        call_context_ = call_context;
        return *this;
    }

    Event& set_object(SEXP r_object) {
        r_object_ = r_object;
        return *this;
    }

    Event& set_expression(SEXP r_expression) {
        r_expression_ = r_expression;
        return *this;
    }

    Type type_;
    SEXP r_call_;
    SEXP r_op_;
    SEXP r_args_;
    SEXP r_rho_;
    SEXP r_variable_;
    SEXP r_value_;
    SEXP r_result_;
    void* call_context_;
    SEXP r_object_;
    SEXP r_expression_;
};

std::string event_type_to_string(const Event::Type& event_type);

#endif /* EVIL_EVENT_H */
