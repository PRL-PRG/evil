#ifndef EVIL_EVENT_H
#define EVIL_EVENT_H

#include <string>

enum class Event {
    ClosureCallEntry,
    VariableDefinition,
    VariableAssignment,
    VariableRemoval,
    VariableLookup
};

std::string event_to_string(const Event& event);

#endif /* EVIL_EVENT_H */
