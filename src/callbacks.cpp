#include "callbacks.h"
#undef length
#include <iostream>
#include "r_init.h"

void increment_counter(ContextSPtr context, int index) {
    SEXP r_data = context->get_data();
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);

    for (int i = 0; i < Rf_length(r_counters); ++i) {
        SEXP r_counter = VECTOR_ELT(r_counters, i);
        int counter_value = asInteger(VECTOR_ELT(r_counter, index));
        SET_VECTOR_ELT(r_counter, index, ScalarInteger(counter_value + 1));
    }
}

void builtin_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counter(context, 0);
    /* check if builtin is .Call and separately count that */
    const char* name = dyntrace_get_c_function_name(r_op);
    if (!strcmp(name, ".Call")) {
        increment_counter(context, 4);
    }
}

void special_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counter(context, 1);
}

void closure_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counter(context, 2);
}

void eval_entry_callback(ContextSPtr context,
                         ApplicationSPtr application,
                         SEXP r_expression,
                         SEXP r_rho) {
    increment_counter(context, 3);
}

void gc_allocation_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            SEXP r_object) {
    increment_counter(context, 5);
}

void variable_definition_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    // std::cerr << "definition" << std::endl;
}

void variable_assignment_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    // std::cerr << "assignment" << std::endl;
}

void variable_removal_callback(ContextSPtr context,
                               ApplicationSPtr application,
                               SEXP r_variable,
                               SEXP r_rho) {
    // std::cerr << "removal" << std::endl;
}

void variable_lookup_callback(ContextSPtr context,
                              ApplicationSPtr application,
                              SEXP r_variable,
                              SEXP r_value,
                              SEXP r_rho) {
    // std::cerr << "lookup" << std::endl;
}
