#include "callbacks.h"
#include "r_init.h"
#include "data.h"

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

    Event event = Event::closure_call_entry(r_call, r_op, r_args, r_rho);

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

    Event event =
        Event::closure_call_exit(r_call, r_op, r_args, r_rho, r_result);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }

    tracer_state.analyze(event);
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
                                  SEXP r_symbol,
                                  SEXP r_value,
                                  SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    FunctionTable& function_table = tracer_state.get_function_table();

    function_table.update(r_value, CHAR(PRINTNAME(r_symbol)), r_rho);

    Event event = Event::variable_definition(r_symbol, r_value, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_assignment_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_symbol,
                                  SEXP r_value,
                                  SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    FunctionTable& function_table = tracer_state.get_function_table();

    function_table.update(r_value, CHAR(PRINTNAME(r_symbol)), r_rho);

    Event event = Event::variable_assignment(r_symbol, r_value, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_removal_callback(ContextSPtr context,
                               ApplicationSPtr application,
                               SEXP r_symbol,
                               SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    Event event = Event::variable_removal(r_symbol, r_rho);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void variable_lookup_callback(ContextSPtr context,
                              ApplicationSPtr application,
                              SEXP r_symbol,
                              SEXP r_value,
                              SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    FunctionTable& function_table = tracer_state.get_function_table();

    function_table.update(r_value, CHAR(PRINTNAME(r_symbol)), r_rho);

    Event event = Event::variable_lookup(r_symbol, r_value, r_rho);

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

    Event event = Event::context_entry(call_context);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void context_exit_callback(ContextSPtr context,
                           ApplicationSPtr application,
                           void* call_context) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    Event event = Event::context_exit(call_context);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
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

    Event event = Event::gc_allocation(r_object);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}

void gc_unmark_callback(ContextSPtr context,
                        ApplicationSPtr application,
                        SEXP r_object) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);

    Event event = Event::gc_unmark(r_object);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}
