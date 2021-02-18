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
    Event event = Event::closure_call_exit(r_call, r_rho, r_result);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
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

void gc_allocation_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            SEXP r_object) {
    SEXP r_data = context->get_data();
    TracerState& tracer_state = *get_tracer_state(r_data);
    Event event = Event::gc_allocation(r_object);

    tracer_state.analyze(event);

    // TODO - add calls to analysis classes
}

void variable_definition_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    SEXP r_data = context->get_data();

    TracerState& tracer_state = *get_tracer_state(r_data);
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
    Event event = Event::variable_lookup(r_variable, r_value, r_rho);

    tracer_state.analyze(event);

    for (Analysis* analysis: get_analyses(r_data)) {
        analysis->analyze(tracer_state, event);
    }
}
