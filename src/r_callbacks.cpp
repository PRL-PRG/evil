#include "r_callbacks.h"
#include "callbacks.h"

SEXP r_get_variable_definition_function() {
    return R_MakeExternalPtr(
        (void*) (variable_definition_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_assignment_function() {
    return R_MakeExternalPtr(
        (void*) (variable_assignment_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_removal_function() {
    return R_MakeExternalPtr(
        (void*) (variable_removal_callback), R_NilValue, R_NilValue);
}

SEXP r_get_variable_lookup_function() {
    return R_MakeExternalPtr(
        (void*) (variable_lookup_callback), R_NilValue, R_NilValue);
}
