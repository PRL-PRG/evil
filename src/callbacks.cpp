#include "callbacks.h"
#undef length
#include <iostream>
#include "r_init.h"

std::unordered_map<SEXP, int> environments_;

int counter_get_call_id(SEXP r_counter) {
    return asInteger(VECTOR_ELT(r_counter, 0));
}

SEXP counter_get_eval_env(SEXP r_counter) {
    return VECTOR_ELT(r_counter, 1);
}

void counter_increment_field(SEXP r_counter, int index) {
    int counter_value = asInteger(VECTOR_ELT(r_counter, index));
    SET_VECTOR_ELT(r_counter, index, ScalarInteger(counter_value + 1));
}

void counter_increment_builtin(SEXP r_counter) {
    counter_increment_field(r_counter, 2);
}

void counter_increment_special(SEXP r_counter) {
    counter_increment_field(r_counter, 3);
}

void counter_increment_closure(SEXP r_counter) {
    counter_increment_field(r_counter, 4);
}

void counter_increment_interpreter_eval(SEXP r_counter) {
    counter_increment_field(r_counter, 5);
}

void counter_increment_c_call(SEXP r_counter) {
    counter_increment_field(r_counter, 6);
}

void counter_increment_allocation(SEXP r_counter) {
    counter_increment_field(r_counter, 7);
}

void counter_increment_direct_writes(SEXP r_counter) {
    counter_increment_field(r_counter, 8);
}

void counter_increment_indirect_writes(SEXP r_counter) {
    counter_increment_field(r_counter, 9);
}

template <typename T>
void increment_counters(ContextSPtr context, T fun) {
    SEXP r_data = context->get_data();
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);

    for (int i = 0; i < Rf_length(r_counters); ++i) {
        SEXP r_counter = VECTOR_ELT(r_counters, i);
        fun(r_counter);
    }
}

void add_environment(ContextSPtr context, SEXP r_env) {
    SEXP r_data = context->get_data();
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);
    SEXP r_counter = VECTOR_ELT(r_counters, Rf_length(r_counters) - 1);
    int eval_call_id = counter_get_call_id(r_counter);

    environments_[r_env] = eval_call_id;
}

/*NOTE: r_variable is an extra argument that is useful for debugging, don't
 * remove it.*/
void check_side_effect(ContextSPtr context, SEXP r_rho, SEXP r_variable) {
    SEXP r_data = context->get_data();
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);
    int stack_size = Rf_length(r_counters);

    /*NOTE: all assignments of a program are stored in direct_writes. This is
     * upper bound used for normalization. */
    counter_increment_direct_writes(VECTOR_ELT(r_counters, 0));

    /*NOTE: all assignments of evals are stored in indirect_writes. This is
     * upper bound used for normalization. */
    if (stack_size > 1) {
        counter_increment_indirect_writes(VECTOR_ELT(r_counters, 0));
    }

    auto result = environments_.find(r_rho);
    /* NOTE: if environment is not present in the map, then it means that it was
     * created before all calls on the stack. Setting it to -1 will have that
     * effect.  */
    int environment_call_id = -1;

    if (result != environments_.end()) {
        environment_call_id = result->second;
    }

    /*NOTE: this flag is used to distinguish direct and indirect side-effects.
     */
    bool first = true;

    /*NOTE: we don't modify counter at position 0 because it is application
     * counter that is handled separately already.*/
    for (int counter_index = stack_size - 1; counter_index > 0;
         --counter_index) {
        SEXP r_counter = VECTOR_ELT(r_counters, counter_index);
        int current_call_id = counter_get_call_id(r_counter);

        if (r_rho == counter_get_eval_env(r_counter) ||
            environment_call_id >= current_call_id) {
            /*NOTE: if this happens, it means the environment was created by a
             * call which happened after the current call. This means the
             * current call is writing to an environemnt which belongs to the
             * computation spawned by it and is not an "external" side-effect.
             */
            break;
        }

        if (first) {
            first = false;
            counter_increment_direct_writes(r_counter);
            // std::cout << ">>>>   " << CHAR(STRING_ELT(r_variable, 0))
            //          << std::endl;
        } else {
            counter_increment_indirect_writes(r_counter);
        }
    }
}

void builtin_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counters(context, counter_increment_builtin);
    /* check if builtin is .Call and separately count that */
    const char* name = dyntrace_get_c_function_name(r_op);
    if (!strcmp(name, ".Call")) {
        increment_counters(context, counter_increment_c_call);
    }
}

void special_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counters(context, counter_increment_special);
}

void closure_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counters(context, counter_increment_closure);
}

void eval_entry_callback(ContextSPtr context,
                         ApplicationSPtr application,
                         SEXP r_expression,
                         SEXP r_rho) {
    increment_counters(context, counter_increment_interpreter_eval);
}

void gc_allocation_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            SEXP r_object) {
    increment_counters(context, counter_increment_allocation);

    if (TYPEOF(r_object) == ENVSXP) {
        add_environment(context, r_object);
    }
}

void variable_definition_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    check_side_effect(context, r_rho, r_variable);
}

void variable_assignment_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho) {
    check_side_effect(context, r_rho, r_variable);
}

void variable_removal_callback(ContextSPtr context,
                               ApplicationSPtr application,
                               SEXP r_variable,
                               SEXP r_rho) {
    check_side_effect(context, r_rho, r_variable);
}

void variable_lookup_callback(ContextSPtr context,
                              ApplicationSPtr application,
                              SEXP r_variable,
                              SEXP r_value,
                              SEXP r_rho) {
    // std::cerr << "lookup" << std::endl;
}
