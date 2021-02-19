#include "r_callbacks.h"
#include "callbacks.h"

SEXP r_get_builtin_call_entry_callback() {
    return R_MakeExternalPtr(
        (void*) (builtin_call_entry_callback), R_NilValue, R_NilValue);
}

SEXP r_get_special_call_entry_callback() {
    return R_MakeExternalPtr(
        (void*) (special_call_entry_callback), R_NilValue, R_NilValue);
}

SEXP r_get_closure_call_entry_callback() {
    return R_MakeExternalPtr(
        (void*) (closure_call_entry_callback), R_NilValue, R_NilValue);
}

SEXP r_get_closure_call_exit_callback() {
    return R_MakeExternalPtr(
        (void*) (closure_call_exit_callback), R_NilValue, R_NilValue);
}

SEXP r_get_eval_entry_callback() {
    return R_MakeExternalPtr(
        (void*) (eval_entry_callback), R_NilValue, R_NilValue);
}

SEXP r_get_gc_allocation_callback() {
    return R_MakeExternalPtr(
        (void*) (gc_allocation_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_definition_callback() {
    return R_MakeExternalPtr(
        (void*) (variable_definition_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_assignment_callback() {
    return R_MakeExternalPtr(
        (void*) (variable_assignment_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_removal_callback() {
    return R_MakeExternalPtr(
        (void*) (variable_removal_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_lookup_callback() {
    return R_MakeExternalPtr(
        (void*) (variable_lookup_callback), R_NilValue, R_NilValue);
}
