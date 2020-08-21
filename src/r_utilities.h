#ifndef EVIL_R_UTILITIES_H
#define EVIL_R_UTILITIES_H

#include <Rincludes.h>

extern "C" {
SEXP sexp_typeof(SEXP x);
SEXP mark_parsed_expression(SEXP x, SEXP parse_fun_name);
}

#endif /* EVIL_R_UTILITIES_H */
