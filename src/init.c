#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include "r_callbacks.h"
#include "r_utilities.h"
#include "r_init.h"
#include "r_data.h"
#include "normalization.h"

SEXP R_ParsedExpressionAttrib = NULL;
SEXP CountersSymbol = NULL;
SEXP PackageSymbol = NULL;
SEXP CharacterDotOnlySymbol = NULL;
SEXP WhichSymbol = NULL;
SEXP NSymbol = NULL;
SEXP FileSymbol = NULL;
SEXP LocalSymbol = NULL;
SEXP AnalysesSymbol = NULL;
SEXP TracerStateSymbol = NULL;

static const R_CallMethodDef callMethods[] = {
    {"sexp_typeof", (DL_FUNC) &sexp_typeof, 1},
    {"mark_parsed_expression", (DL_FUNC) &mark_parsed_expression, 2},
    {"get_ast_size", (DL_FUNC) &r_get_ast_size, 1},
    {"get_builtin_call_entry_callback", (DL_FUNC) &r_get_builtin_call_entry_callback, 0},
    {"get_special_call_entry_callback", (DL_FUNC) &r_get_special_call_entry_callback, 0},
    {"get_closure_call_entry_callback", (DL_FUNC) &r_get_closure_call_entry_callback, 0},
    {"get_closure_call_exit_callback", (DL_FUNC) &r_get_closure_call_exit_callback, 0},
    {"get_eval_entry_callback", (DL_FUNC) &r_get_eval_entry_callback, 0},
    {"get_variable_definition_callback", (DL_FUNC) &r_get_variable_definition_callback, 0},
    {"get_variable_assignment_callback", (DL_FUNC) &r_get_variable_assignment_callback, 0},
    {"get_variable_removal_callback", (DL_FUNC) &r_get_variable_removal_callback, 0},
    {"get_variable_lookup_callback", (DL_FUNC) &r_get_variable_lookup_callback, 0},
    {"get_context_entry_callback", (DL_FUNC) &r_get_context_entry_callback, 0},
    {"get_context_exit_callback", (DL_FUNC) &r_get_context_exit_callback, 0},
    {"get_context_jump_callback", (DL_FUNC) &r_get_context_jump_callback, 0},
    {"get_gc_allocation_callback", (DL_FUNC) &r_get_gc_allocation_callback, 0},
    {"get_gc_unmark_callback", (DL_FUNC) &r_get_gc_unmark_callback, 0},
    {"tracer_data_initialize", (DL_FUNC) &r_tracer_data_initialize, 1},
    {"function_table_initialize", (DL_FUNC) &r_function_table_initialize, 1},
    {"tracer_data_finalize", (DL_FUNC) &r_tracer_data_finalize, 1},
    {"tracer_data_eval_call_entry", (DL_FUNC) &r_tracer_data_eval_call_entry, 4},
    {"tracer_data_eval_call_exit", (DL_FUNC) &r_tracer_data_eval_call_exit, 1},
    {"tracer_data_add_package", (DL_FUNC) &r_tracer_data_add_package, 2},
    {"normalize", (DL_FUNC) &r_normalize, 3},
    {"normalize_expr", (DL_FUNC) &r_normalize_expr, 1},
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
    FileSymbol = Rf_install("file");
    LocalSymbol = Rf_install("local");
    AnalysesSymbol = Rf_install("tables");
    TracerStateSymbol = Rf_install("tracer_state");
}
