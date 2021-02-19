#include "callbacks.h"
#include "r_init.h"
#include "data.h"

void update_function_name(TracerState& tracer_state,
                          SEXP r_value,
                          SEXP r_variable,
                          SEXP r_rho) {
    SEXP r_op = NULL;

    if (TYPEOF(r_value) == PROMSXP) {
        r_op = dyntrace_get_promise_value(r_value);
        if (r_op == R_UnboundValue || TYPEOF(r_op) != CLOSXP) {
            r_op = dyntrace_get_promise_expression(r_value);
            if (r_op == R_UnboundValue || TYPEOF(r_op) != CLOSXP) {
                return;
            }
        }
    } else if (TYPEOF(r_value) == CLOSXP) {
        r_op = r_value;
    } else {
        return;
    }

    FunctionTable& function_table = tracer_state.get_function_table();
    function_table.update(r_op, r_variable, r_rho);
}

void builtin_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    /* check if builtin is .Call and separately count that
       const char* name = dyntrace_get_c_function_name(r_op); */
}

void special_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
}

void closure_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    SEXP r_data = context->get_data();
    TracerState& tracer_state = *get_tracer_state(r_data);

    FunctionTable& function_table = tracer_state.get_function_table();

    Stack& stack = tracer_state.get_stack();

    Function* function = function_table.lookup(r_op, r_call, r_rho);

    StackFrame frame =
        StackFrame::from_call(new Call(function, r_call, r_args, r_rho));

    stack.push(frame);

    Event event = Event::closure_call_entry(r_call, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void closure_call_exit_callback(ContextSPtr context,
                                ApplicationSPtr application,
                                SEXP r_call,
                                SEXP r_op,
                                SEXP r_args,
                                SEXP r_rho,
                                SEXP r_result) {
    SEXP r_data = context->get_data();
    TracerState& tracer_state = *get_tracer_state(r_data);

    Stack& stack = tracer_state.get_stack();
    StackFrame& frame = stack.peek();
    Call* call = nullptr;

    if (!frame.is_call()) {
        Rf_error("mismatched stack frame, expected call got context");
    } else {
        call = frame.as_call();
        if (call->get_expression() != r_call ||
            call->get_arguments() != r_args ||
            call->get_environment() != r_rho) {
            Rf_error("mismatched call on stack");
        }
    }

    Event event = Event::closure_call_exit(r_call, r_rho, r_result);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }

    stack.pop();

    delete call;
}

void eval_entry_callback(ContextSPtr context,
                         ApplicationSPtr application,
                         SEXP r_expression,
                         SEXP r_rho) {
    SEXP r_data = context->get_data();
    TracerState& tracer_state = *get_tracer_state(r_data);
    Event event = Event::eval_entry(r_expression, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_definition_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    update_function_name(tracer_state, r_value, r_variable, r_rho);

    Event event = Event::variable_definition(r_variable, r_value, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_assignment_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    update_function_name(tracer_state, r_value, r_variable, r_rho);

    Event event = Event::variable_assignment(r_variable, r_value, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_removal_callback(ContextSPtr context,
                               ApplicationSPtr application,
                               SEXP r_variable,
                               SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    Event event = Event::variable_removal(r_variable, r_rho);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_lookup_callback(ContextSPtr context,
                              ApplicationSPtr application,
                              SEXP r_variable,
                              SEXP r_value,
                              SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    update_function_name(tracer_state, r_value, r_variable, r_rho);

    Event event = Event::variable_lookup(r_variable, r_value, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void context_entry_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            void* call_context) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    StackFrame frame = StackFrame::from_context(call_context);
    Stack& stack = tracer_state.get_stack();
    stack.push(frame);
}

void context_exit_callback(ContextSPtr context,
                           ApplicationSPtr application,
                           void* call_context) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    Stack& stack = tracer_state.get_stack();
    StackFrame frame = stack.pop();

    if (!frame.is_context()) {
        Rf_error("mismatched stack frame, expected context got call");
    } else if (frame.as_context() != call_context) {
        Rf_error("mismatched context on stack, expected %p got %p",
                 call_context,
                 frame.as_context());
    }
}

void context_jump_callback(ContextSPtr context,
                           ApplicationSPtr application,
                           void* call_context) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    Stack& stack = tracer_state.get_stack();

    while (stack.size() > 0) {
        StackFrame& frame = stack.peek();

        if (frame.is_context()) {
            if (frame.as_context() == call_context) {
                return;
            }

            else {
                void* call_context = frame.as_context();
                context_exit_callback(context, application, call_context);
            }
        }

        else if (frame.is_call()) {
            Call* call = frame.as_call();

            SEXP r_call = call->get_expression();
            SEXP r_op = call->get_function()->get_op();
            SEXP r_args = call->get_arguments();
            SEXP r_rho = call->get_environment();

            if (call->get_function()->get_type() == CLOSXP) {
                closure_call_exit_callback(
                    context, application, r_call, r_op, r_args, r_rho, NULL);
            }
        }
    }

    Rf_error("cannot find matching context while unwinding\n");
}

void gc_allocation_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            SEXP r_object) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    if (TYPEOF(r_object) == CLOSXP) {
        FunctionTable& function_table = tracer_state.get_function_table();

        function_table.insert(r_object);
    }

    Event event = Event::gc_allocation(r_object);

    tracer_state.analyze(event);
}

void gc_unmark_callback(ContextSPtr context,
                        ApplicationSPtr application,
                        SEXP r_object) {
    if (TYPEOF(r_object) == CLOSXP) {
        SEXP r_data = context->get_data();

        TracerState& tracer_state = *get_tracer_state(r_data);

        FunctionTable& function_table = tracer_state.get_function_table();

        function_table.remove(r_object);
    }
}
