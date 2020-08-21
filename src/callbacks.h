#ifndef EVIL_CALLBACKS_H
#define EVIL_CALLBACKS_H

#    include "Context.hpp"
#    include "Application.hpp"

using instrumentr::ApplicationSPtr;
using instrumentr::ContextSPtr;

void variable_definition_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho);

void variable_assignment_callback(ContextSPtr context,
                                  ApplicationSPtr application,
                                  SEXP r_variable,
                                  SEXP r_value,
                                  SEXP r_rho);

void variable_removal_callback(ContextSPtr context,
                               ApplicationSPtr application,
                               SEXP r_variable,
                               SEXP r_rho);

void variable_lookup_callback(ContextSPtr context,
                              ApplicationSPtr application,
                              SEXP r_variable,
                              SEXP r_value,
                              SEXP r_rho);

#endif /* EVIL_CALLBACKS_H */
