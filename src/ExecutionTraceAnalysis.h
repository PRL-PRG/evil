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
            table_.record(depth_, "ent", get_full_function_name_(function));
            ++depth_;
        }

        else if (event_type == Event::Type::ClosureCallExit) {
            --depth_;
            const Stack& stack = tracer_state.get_stack();
            const StackFrame& frame = stack.peek();
            const Function* function = frame.as_call()->get_function();
            table_.record(depth_, "ext", get_full_function_name_(function));
        }

        else if (event_type == Event::Type::VariableDefinition ||
                 event_type == Event::Type::VariableAssignment) {
            std::string varname = CHAR(PRINTNAME(event.get_variable()));
            /* ignore *tmp* used by R internals for intermediate computation */
            if (is_tmp_val_symbol_(varname)) {
                return;
            }
            table_.record(depth_, event.get_short_name(), varname);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&table_};
    }

  private:
    ExecutionTraceTable table_;
    int depth_;

    std::string get_full_function_name_(const Function* function) {
        std::string full_name("");

        if (function->has_package_name()) {
            full_name.append(function->get_package_name());
            full_name.append("::");
        }

        std::string name("<unknown>");

        if (function->has_name()) {
            name = function->get_name();
        }

        full_name.append(name);

        return full_name;
    }

    bool is_tmp_val_symbol_(const std::string& name) {
        return name == "*tmp*";
    }
};

#endif /* EVIL_EXECUTION_TRACE_ANALYSIS_H */
