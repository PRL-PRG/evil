#ifndef EVIL_DATA_H
#define EVIL_DATA_H

#include <R.h>
#include <vector>
#include "Analysis.h"
#include "TracerState.h"

std::vector<Analysis*> get_analyses(SEXP r_data);
TracerState* get_tracer_state(SEXP r_data);

#endif /* EVIL_DATA_H */
