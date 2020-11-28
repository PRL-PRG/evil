#ifndef EVIL_R_REFLECTION_H
#define EVIL_R_REFLECTION_H

#include <R.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP r_create_reflection_table();

SEXP r_reflection_table_to_data_frame(SEXP r_reflection_table);

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_REFLECTION_H */
