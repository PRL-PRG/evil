#ifndef EVIL_REFLECTION_H
#define EVIL_REFLECTION_H

#include "ReflectionTable.h"

void inspect_for_reflective_call(ReflectionTable* reflection_table,
                                 SEXP r_call,
                                 SEXP r_rho,
                                 int call_id,
                                 int eval_frame_position,
                                 int current_frame_position);
#endif /* EVIL_REFLECTION_H */
