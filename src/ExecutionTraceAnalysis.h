#ifndef EVIL_EXECUTION_TRACE_ANALYSIS_H
#define EVIL_EXECUTION_TRACE_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "ExecutionTraceTable.h"

class ExecutionTraceAnalysis: public Analysis {
  public:
    ExecutionTraceAnalysis(): Analysis(), depth_(0) {
    }

    void analyze(TracerState& tracer_state, Event& event) override {
        Event::Type event_type = event.get_type();

        if (event_type == Event::Type::ClosureCallEntry) {
            const Stack& stack = tracer_state.get_stack();
            const StackFrame& frame = stack.peek();
            const Function* function = frame.as_call()->get_function();
            table_.record(depth_, "ent", function->get_name());
            ++depth_;
        }

        else if (event_type == Event::Type::ClosureCallExit) {
            --depth_;
            const Stack& stack = tracer_state.get_stack();
            const StackFrame& frame = stack.peek();
            const Function* function = frame.as_call()->get_function();
            table_.record(depth_, "ext", function->get_name());
        }

        else if (event_type == Event::Type::VariableDefinition) {
            std::string varname = CHAR(STRING_ELT(event.get_variable(), 0));
            table_.record(depth_, "def", varname);
        }

        else if (event_type == Event::Type::VariableAssignment) {
            std::string varname = CHAR(STRING_ELT(event.get_variable(), 0));
            table_.record(depth_, "asn", varname);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&table_};
    }

  private:
    ExecutionTraceTable table_;
    int depth_;
};

#endif /* EVIL_EXECUTION_TRACE_ANALYSIS_H */
