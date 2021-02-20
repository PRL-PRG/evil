#include "Event.h"

std::string event_type_to_string(const Event::Type& event_type) {
    switch (event_type) {
    case Event::Type::EvalEntry:
        return "eve";
    case Event::Type::ClosureCallEntry:
        return "ent";
    case Event::Type::ClosureCallExit:
        return "ext";
    case Event::Type::VariableDefinition:
        return "def";
    case Event::Type::VariableAssignment:
        return "asn";
    case Event::Type::VariableRemoval:
        return "rvl";
    case Event::Type::VariableLookup:
        return "lkp";
    case Event::Type::GcAllocation:
        return "gca";
    }
    return "unhandled";
}

std::string Event::get_short_name() const {
    return event_type_to_string(type_);
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

Event Event::gc_allocation(SEXP r_object) {
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

Event Event::eval_entry(SEXP r_expression, SEXP r_rho) {
    return Event(Event::Type::EvalEntry)
        .set_expression(r_expression)
        .set_rho(r_rho);
}
