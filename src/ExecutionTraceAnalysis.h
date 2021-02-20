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
            const Call* call = frame.as_call();
            const Function* function = call->get_function();
            table_.record(
                depth_, call->get_id(), "ent", function->get_qualified_name());
            ++depth_;
        }

        else if (event_type == Event::Type::ClosureCallExit) {
            --depth_;
            const Stack& stack = tracer_state.get_stack();
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            const Function* function = call->get_function();
            table_.record(
                depth_, call->get_id(), "ext", function->get_qualified_name());
        }

        else if (event_type == Event::Type::VariableDefinition ||
                 event_type == Event::Type::VariableAssignment) {
            std::string varname = CHAR(PRINTNAME(event.get_variable()));
            /* ignore *tmp* used by R internals for intermediate computation */
            if (is_tmp_val_symbol_(varname)) {
                return;
            }
            table_.record(depth_, NA_INTEGER, event.get_short_name(), varname);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&table_};
    }

  private:
    ExecutionTraceTable table_;
    int depth_;

    bool is_tmp_val_symbol_(const std::string& name) {
        return name == "*tmp*";
    }
};

#endif /* EVIL_EXECUTION_TRACE_ANALYSIS_H */
