#ifndef EVIL_CALLBACKS_H
#define EVIL_CALLBACKS_H

#include "Context.hpp"
#include "Application.hpp"

using instrumentr::ApplicationSPtr;
using instrumentr::ContextSPtr;

void builtin_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho);

void special_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho);

void closure_call_entry_callback(ContextSPtr context,
                                 ApplicationSPtr application,
                                 SEXP r_call,
                                 SEXP r_op,
                                 SEXP r_args,
                                 SEXP r_rho);

void closure_call_exit_callback(ContextSPtr context,
                                ApplicationSPtr application,
                                SEXP r_call,
                                SEXP r_op,
                                SEXP r_args,
                                SEXP r_rho,
                                SEXP r_result);

void eval_entry_callback(ContextSPtr context,
                         ApplicationSPtr application,
                         SEXP r_expression,
                         SEXP r_rho);

void gc_allocation_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            SEXP r_object);

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

void context_entry_callback(ContextSPtr context,
                            ApplicationSPtr application,
                            void* call_context);

void context_exit_callback(ContextSPtr context,
                           ApplicationSPtr application,
                           void* call_context);

void context_jump_callback(ContextSPtr context,
                           ApplicationSPtr application,
                           void* call_context);

#endif /* EVIL_CALLBACKS_H */
