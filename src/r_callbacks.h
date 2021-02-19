#ifndef EVIL_R_CALLBACKS_H
#define EVIL_R_CALLBACKS_H

#include "Rincludes.h"

#ifdef __cplusplus
extern "C" {
#endif
SEXP r_get_builtin_call_entry_callback();
SEXP r_get_special_call_entry_callback();
SEXP r_get_closure_call_entry_callback();
SEXP r_get_closure_call_exit_callback();
SEXP r_get_eval_entry_callback();
SEXP r_get_gc_allocation_callback();
SEXP r_get_variable_definition_callback();
SEXP r_get_variable_assignment_callback();
SEXP r_get_variable_removal_callback();
SEXP r_get_variable_lookup_callback();

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_CALLBACKS_H  */
