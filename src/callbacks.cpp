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

bool is_call_to(const char* function_name, SEXP r_call) {
    SEXP r_function_name = CAR(r_call);
    bool library =
        TYPEOF(r_function_name) == SYMSXP &&
        (strcmp(function_name, CHAR(PRINTNAME(r_function_name))) == 0);
    return library;
}

const char* get_package_name(SEXP r_call, SEXP r_rho) {
    /* if package is not provided, then we return null  */
    if (CADR(r_call) == R_MissingArg) {
        return nullptr;
    }

    bool character_only = asLogical(
        Rf_eval(Rf_findVarInFrame(r_rho, CharacterDotOnlySymbol), r_rho));

    SEXP r_package_name_promise = Rf_findVarInFrame(r_rho, PackageSymbol);

    /*  if character.only is true, then package is a symbol bound to a string */
    if (character_only) {
        SEXP r_package_name = Rf_eval(r_package_name_promise, r_rho);
        if (TYPEOF(r_package_name) == STRSXP &&
            STRING_ELT(r_package_name, 0) != NA_STRING) {
            return CHAR(STRING_ELT(r_package_name, 0));
        }
    } else {
        SEXP r_package_name =
            dyntrace_get_promise_expression(r_package_name_promise);
        if (TYPEOF(r_package_name) == SYMSXP) {
            return CHAR(PRINTNAME(r_package_name));
        } else if (TYPEOF(r_package_name) == STRSXP &&
                   STRING_ELT(r_package_name, 0) != NA_STRING) {
            return CHAR(STRING_ELT(r_package_name, 0));
        }
    }

    return "???";
}

int get_which(SEXP r_call, SEXP r_rho) {
    /* if package is not provided, then we return null  */
    if (CADR(r_call) == R_MissingArg) {
        return -1;
    }

    SEXP r_which_promise = Rf_findVarInFrame(r_rho, WhichSymbol);

    SEXP r_which = Rf_eval(r_which_promise, r_rho);

    if (TYPEOF(r_which) == INTSXP) {
        return INTEGER(r_which)[0];
    } else if (TYPEOF(r_which) == REALSXP) {
        return (int) (REAL(r_which)[0]);
    } else {
        return -1;
    }
}

void closure_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho) {
    SEXP r_data = context->get_data();
    SEXP r_counters = Rf_findVarInFrame(r_data, CountersSymbol);
    SEXP r_counter = VECTOR_ELT(r_counters, Rf_length(r_counters) - 1);

    if (is_call_to("library", r_call)) {
        const char* package_name = get_package_name(r_call, r_rho);

        if (package_name != nullptr) {
            counter_add_package(r_counters, 16, package_name);
        }
    }

    if (is_call_to("require", r_call)) {
        const char* package_name = get_package_name(r_call, r_rho);

        if (package_name != nullptr) {
            counter_add_package(r_counters, 17, package_name);
        }
    }

    if (is_call_to("sys.calls", r_call)) {
        counter_increment_field(r_counter, 18);
    }

    if (is_call_to("sys.frames", r_call)) {
        counter_increment_field(r_counter, 19);
    }

    if (is_call_to("sys.parents", r_call)) {
        counter_increment_field(r_counter, 20);
    }

    if (is_call_to("sys.frame", r_call)) {
        int which_value = get_which(r_call, r_rho);

        if (which_value != 0) {
            counter_add_which(r_counter, 21, which_value);
        }
    }

    if (is_call_to("sys.call", r_call)) {
        int which_value = get_which(r_call, r_rho);

        if (which_value != 0) {
            counter_add_which(r_counter, 22, which_value);
        }
    }

    if (is_call_to("sys.function", r_call)) {
        int which_value = get_which(r_call, r_rho);

        if (which_value != 0) {
            counter_add_which(r_counter, 23, which_value);
        }
    }

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
