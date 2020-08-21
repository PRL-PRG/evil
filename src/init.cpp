#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include "r_callbacks.h"
#include "r_utilities.h"
#include "r_init.h"


SEXP R_ParsedExpressionAttrib = NULL;

static const R_CallMethodDef callMethods[] = {
    {"sexp_typeof", (DL_FUNC) &sexp_typeof, 1},
    {"mark_parsed_expression", (DL_FUNC) &mark_parsed_expression, 2},
    {"get_variable_definition_function", (DL_FUNC) &r_get_variable_definition_function, 0},
    {"get_variable_assignment_function", (DL_FUNC) &r_get_variable_assignment_function, 0},
    {"get_variable_removal_function", (DL_FUNC) &r_get_variable_removal_function, 0},
    {"get_variable_lookup_function", (DL_FUNC) &r_get_variable_lookup_function, 0},
    {NULL, NULL, 0}
};

void R_init_evil(DllInfo* dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);

    R_useDynamicSymbols(dll, FALSE);

    R_ParsedExpressionAttrib = Rf_install("._evil_parsed_expression");
}
