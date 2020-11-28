#include "callbacks.h"
#include "r_init.h"
#include "data.h"
#include "r_utilities.h"


std::unordered_map<SEXP, int> environments_;

int counter_get_call_id(SEXP r_counter) {
    return asInteger(VECTOR_ELT(r_counter, 0));
}

SEXP counter_get_eval_env(SEXP r_counter) {
    return VECTOR_ELT(r_counter, 1);
}

int counter_get_eval_frame_depth(SEXP r_counter) {
    return asInteger(VECTOR_ELT(r_counter, 18));
}

void counter_increment_field(SEXP r_counter, int index) {
    int counter_value = asInteger(VECTOR_ELT(r_counter, index));
    SET_VECTOR_ELT(r_counter, index, ScalarInteger(counter_value + 1));
}

void counter_increment_direct_builtin(SEXP r_counter) {
    counter_increment_field(r_counter, 2);
}

void counter_increment_indirect_builtin(SEXP r_counter) {
    counter_increment_field(r_counter, 3);
}

void counter_increment_direct_special(SEXP r_counter) {
    counter_increment_field(r_counter, 4);
}

void counter_increment_indirect_special(SEXP r_counter) {
    counter_increment_field(r_counter, 5);
}

void counter_increment_direct_closure(SEXP r_counter) {
    counter_increment_field(r_counter, 6);
}

void counter_increment_indirect_closure(SEXP r_counter) {
    counter_increment_field(r_counter, 7);
}

void counter_increment_direct_interpreter_eval(SEXP r_counter) {
    counter_increment_field(r_counter, 8);
}

void counter_increment_indirect_interpreter_eval(SEXP r_counter) {
    counter_increment_field(r_counter, 9);
}

void counter_increment_direct_c_call(SEXP r_counter) {
    counter_increment_field(r_counter, 10);
}

void counter_increment_indirect_c_call(SEXP r_counter) {
    counter_increment_field(r_counter, 11);
}

void counter_increment_direct_allocation(SEXP r_counter) {
    counter_increment_field(r_counter, 12);
}

void counter_increment_indirect_allocation(SEXP r_counter) {
    counter_increment_field(r_counter, 13);
}

void counter_increment_direct_writes(SEXP r_counter) {
    counter_increment_field(r_counter, 14);
}

void counter_increment_indirect_writes(SEXP r_counter) {
    counter_increment_field(r_counter, 15);
}

void counter_add_package(SEXP r_counters, int index, const char* new_package) {
    int stack_size = Rf_length(r_counters);
    SEXP r_counter = VECTOR_ELT(r_counters, stack_size - 1);

    SEXP r_old_packages = VECTOR_ELT(r_counter, index);
    const char* old_package = CHAR(STRING_ELT(r_old_packages, 0));
    SEXP r_all_packages = R_NilValue;
    /* NOTE: this is always a string of size 1, either "" or
     * "package_1;package_2;...;package_N"  */

    if (strcmp(old_package, "") == 0) {
        r_all_packages = mkString(new_package);
    } else {
        std::string all_packages =
            std::string(old_package) + std::string(";") + new_package;
        r_all_packages = mkString(all_packages.c_str());
    }

    SET_VECTOR_ELT(r_counter, index, r_all_packages);
}

void counter_add_which(SEXP r_counter, int index, int new_which) {
    std::string new_which_str = std::to_string(new_which);
    SEXP r_old_whiches = VECTOR_ELT(r_counter, index);
    const char* old_which = CHAR(STRING_ELT(r_old_whiches, 0));
    SEXP r_all_whiches = R_NilValue;
    /* NOTE: this is always a string of size 1, either "" or
     * "which_1;which_2;...;which_N"  */

    if (strcmp(old_which, "") == 0) {
        r_all_whiches = mkString(new_which_str.c_str());
    } else {
        std::string all_whiches =
            std::string(old_which) + std::string(";") + new_which_str;
        r_all_whiches = mkString(all_whiches.c_str());
    }

    SET_VECTOR_ELT(r_counter, index, r_all_whiches);
}

template <typename T>
void increment_counters(ContextSPtr context, T direct_fun, T indirect_fun) {
    SEXP r_data = context->get_data();
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);
    int stack_size = Rf_length(r_counters);

    bool direct = true;

    for (int i = stack_size - 1; i >= 0; --i) {
        SEXP r_counter = VECTOR_ELT(r_counters, i);

        if (direct) {
            direct = false;
            direct_fun(r_counter);
        } else {
            indirect_fun(r_counter);
        }
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
    increment_counters(context,
                       counter_increment_direct_builtin,
                       counter_increment_indirect_builtin);
    /* check if builtin is .Call and separately count that */
    const char* name = dyntrace_get_c_function_name(r_op);
    if (!strcmp(name, ".Call")) {
        increment_counters(context,
                           counter_increment_direct_c_call,
                           counter_increment_indirect_c_call);
    }
}

void special_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    increment_counters(context,
                       counter_increment_direct_special,
                       counter_increment_indirect_special);
}

void closure_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    SEXP r_data = context->get_data();
    ReflectionTable* reflection_table = get_table<ReflectionTable>(r_data);
    CodeTable* code_table = get_table<CodeTable>(r_data);
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);
    SEXP r_counter = VECTOR_ELT(r_counters, Rf_length(r_counters) - 1);
    int call_id = counter_get_call_id(r_counter);
    int eval_frame_depth = counter_get_eval_frame_depth(r_counter);


    CodeTable::inspect_and_record(code_table,
                                      r_call,
                                      r_rho,
                                      call_id);

    ReflectionTable::inspect_and_record(reflection_table,
                                            r_call,
                                            r_rho,
                                            call_id,
                                            eval_frame_depth,
                                            dyntrace_get_frame_depth());

    increment_counters(context,
                       counter_increment_direct_closure,
                       counter_increment_indirect_closure);
}

void eval_entry_callback(ContextSPtr context,
                         ApplicationSPtr application,
                         SEXP r_expression,
                         SEXP r_rho) {
    increment_counters(context,
                       counter_increment_direct_interpreter_eval,
                       counter_increment_indirect_interpreter_eval);
}

void gc_allocation_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            SEXP r_object) {
    increment_counters(context,
                       counter_increment_direct_allocation,
                       counter_increment_indirect_allocation);

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
