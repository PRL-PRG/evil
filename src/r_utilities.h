#ifndef EVIL_R_UTILITIES_H
#define EVIL_R_UTILITIES_H

#include <Rincludes.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP sexp_typeof(SEXP x);
SEXP mark_parsed_expression(SEXP x, SEXP parse_fun_name);
SEXP r_get_ast_size(SEXP ast);
SEXP r_get_argument(SEXP r_call, SEXP r_rho, SEXP r_argument_name, int evaluate);
int get_argument_as_integer(SEXP r_call, SEXP r_rho, SEXP r_argument_name);
int is_call_to(const char* function_name, SEXP r_call);

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_UTILITIES_H */
