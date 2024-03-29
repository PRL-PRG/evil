#ifndef EVIL_R_UTILITIES_H
#define EVIL_R_UTILITIES_H

#include <Rincludes.h>

extern const char* MissingStringValue;
extern int r_tracing;

#ifdef __cplusplus
extern "C" {
#endif

SEXP sexp_typeof(SEXP x);
SEXP mark_parsed_expression(SEXP x, SEXP parse_fun_name);
SEXP r_get_ast_size(SEXP ast);
int get_sexp_type(SEXP r_value, int follow_symbol);

SEXP in_r_tracing(SEXP tracing);

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_UTILITIES_H */
