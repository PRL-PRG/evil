#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include "r_callbacks.h"
#include "r_utilities.h"
#include "r_init.h"
#include "r_reflection.h"

SEXP R_ParsedExpressionAttrib = NULL;
SEXP CountersSymbol = NULL;
SEXP PackageSymbol = NULL;
SEXP CharacterDotOnlySymbol = NULL;
SEXP WhichSymbol = NULL;
SEXP NSymbol = NULL;
SEXP ReflectionTableSymbol = NULL;

static const R_CallMethodDef callMethods[] = {
    {"sexp_typeof", (DL_FUNC) &sexp_typeof, 1},
    {"mark_parsed_expression", (DL_FUNC) &mark_parsed_expression, 2},
    {"get_ast_size", (DL_FUNC) &r_get_ast_size, 1},
    {"get_builtin_call_entry_callback", (DL_FUNC) &r_get_builtin_call_entry_callback, 0},
    {"get_special_call_entry_callback", (DL_FUNC) &r_get_special_call_entry_callback, 0},
    {"get_closure_call_entry_callback", (DL_FUNC) &r_get_closure_call_entry_callback, 0},
    {"get_eval_entry_callback", (DL_FUNC) &r_get_eval_entry_callback, 0},
    {"get_gc_allocation_callback", (DL_FUNC) &r_get_gc_allocation_callback, 0},
    {"get_variable_definition_callback", (DL_FUNC) &r_get_variable_definition_callback, 0},
    {"get_variable_assignment_callback", (DL_FUNC) &r_get_variable_assignment_callback, 0},
    {"get_variable_removal_callback", (DL_FUNC) &r_get_variable_removal_callback, 0},
    {"get_variable_lookup_callback", (DL_FUNC) &r_get_variable_lookup_callback, 0},
    {"create_reflection_table", (DL_FUNC) &r_create_reflection_table, 0},
    {"reflection_table_to_data_frame", (DL_FUNC) &r_reflection_table_to_data_frame, 1},
    {NULL, NULL, 0}
};

void R_init_evil(DllInfo* dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);

    R_useDynamicSymbols(dll, FALSE);

    R_ParsedExpressionAttrib = Rf_install("._evil_parsed_expression");

    CountersSymbol = Rf_install("counters");
    PackageSymbol = Rf_install("package");
    CharacterDotOnlySymbol = Rf_install("character.only");
    WhichSymbol = Rf_install("which");
    NSymbol = Rf_install("n");
    ReflectionTableSymbol = Rf_install("reflection_table");
}
