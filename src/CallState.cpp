#include "CallState.h"

CallState CallState::closure_call_entry(int call_id,
                                        SEXP r_call,
                                        SEXP r_rho,
                                        int eval_frame_depth) {
    return CallState(
        Event::ClosureCallEntry, call_id, r_call, r_rho, eval_frame_depth);
}

CallState CallState::variable_definition(int call_id,
                                         SEXP r_variable,
                                         SEXP r_value,
                                         SEXP r_rho) {
    return CallState(
        Event::VariableDefinition, call_id, r_variable, r_value, r_rho);
}

CallState CallState::variable_assignment(int call_id,
                                         SEXP r_variable,
                                         SEXP r_value,
                                         SEXP r_rho) {
    return CallState(
        Event::VariableAssignment, call_id, r_variable, r_value, r_rho);
}

CallState
CallState::variable_removal(int call_id, SEXP r_variable, SEXP r_rho) {
    return CallState(Event::VariableRemoval, call_id, r_variable, r_rho);
}

CallState CallState::variable_lookup(int call_id,
                                     SEXP r_variable,
                                     SEXP r_value,
                                     SEXP r_rho) {
    return CallState(
        Event::VariableLookup, call_id, r_variable, r_value, r_rho);
}
