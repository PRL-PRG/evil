#include "Event.h"

std::string event_to_string(const Event& event) {
    switch (event) {
    case Event::ClosureCallEntry:
        return "closure_call_entry";
    case Event::VariableDefinition:
        return "variable_definition";
    case Event::VariableAssignment:
        return "variable_assignment";
    case Event::VariableRemoval:
        return "variable_removal";
    case Event::VariableLookup:
        return "variable_lookup";
    }

    return "unhandled";
}
