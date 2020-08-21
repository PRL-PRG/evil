#ifndef EVIL_R_CALLBACKS_H
#define EVIL_R_CALLBACKS_H

#include "Rincludes.h"

extern "C" {
SEXP r_get_variable_definition_function();
SEXP r_get_variable_assignment_function();
SEXP r_get_variable_removal_function();
SEXP r_get_variable_lookup_function();
}

#endif /* EVIL_R_CALLBACKS_H  */
