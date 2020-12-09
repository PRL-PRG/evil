#ifndef EVIL_R_DATA_H
#define EVIL_R_DATA_H

#include <Rinternals.h>
#undef length

#ifdef __cplusplus
extern "C" {
#endif

SEXP r_initialize_analyses(SEXP r_data);
SEXP r_get_tables(SEXP r_data);

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_DATA_H */
