#include "Event.h"

std::string event_type_to_string(const Event::Type& event_type) {
    switch (event_type) {
    case Event::Type::ClosureCallEntry:
        return "closure_call_entry";
    case Event::Type::ClosureCallExit:
        return "closure_call_exit";
    case Event::Type::GcAllocation:
        return "gc_allocation";
    case Event::Type::VariableDefinition:
        return "variable_definition";
    case Event::Type::VariableAssignment:
        return "variable_assignment";
    case Event::Type::VariableRemoval:
        return "variable_removal";
    case Event::Type::VariableLookup:
        return "variable_lookup";
    }

    return "unhandled";
}

Event Event::closure_call_entry(SEXP r_call, SEXP r_rho) {
    return Event(Event::Type::ClosureCallEntry).set_call(r_call).set_rho(r_rho);
}

Event Event::closure_call_exit(SEXP r_call, SEXP r_rho, SEXP r_result) {
    return Event(Event::Type::ClosureCallExit)
        .set_call(r_call)
        .set_rho(r_rho)
        .set_result(r_result);
}

Event Event::gc_allocation(SEXP r_object){
    return Event(Event::Type::GcAllocation).set_object(r_object);
}

Event Event::variable_definition(SEXP r_variable, SEXP r_value, SEXP r_rho) {
    return Event(Event::Type::VariableDefinition)
        .set_variable(r_variable)
        .set_value(r_value)
        .set_rho(r_rho);
}

Event Event::variable_assignment(SEXP r_variable, SEXP r_value, SEXP r_rho) {
    return Event(Event::Type::VariableAssignment)
        .set_variable(r_variable)
        .set_value(r_value)
        .set_rho(r_rho);
}

Event Event::variable_removal(SEXP r_variable, SEXP r_rho) {
    return Event(Event::Type::VariableRemoval)
        .set_variable(r_variable)
        .set_rho(r_rho);
}

Event Event::variable_lookup(SEXP r_variable, SEXP r_value, SEXP r_rho) {
    return Event(Event::Type::VariableLookup)
        .set_variable(r_variable)
        .set_value(r_value)
        .set_rho(r_rho);
}
