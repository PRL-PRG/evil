#ifndef EVIL_R_DATA_H
#define EVIL_R_DATA_H

#include <Rinternals.h>
#undef length

#ifdef __cplusplus
extern "C" {
#endif

SEXP r_tracer_data_initialize(SEXP r_data);
SEXP r_tracer_data_finalize(SEXP r_data);
SEXP r_tracer_data_eval_call_entry(SEXP r_data,
                                   SEXP r_call_id,
                                   SEXP r_env,
                                   SEXP r_frame_depth);
SEXP r_tracer_data_eval_call_exit(SEXP r_data);
SEXP r_function_table_initialize(SEXP r_data);

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_DATA_H */
