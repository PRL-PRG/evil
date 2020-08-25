#ifndef EVIL_R_UTILITIES_H
#define EVIL_R_UTILITIES_H

#include <Rincludes.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP sexp_typeof(SEXP x);
SEXP mark_parsed_expression(SEXP x, SEXP parse_fun_name);

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_UTILITIES_H */
