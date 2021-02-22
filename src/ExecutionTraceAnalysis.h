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
        Stack& stack = tracer_state.get_stack();
        EnvironmentTable& environment_table =
            tracer_state.get_environment_table();

        /* ignore side effects happening in package functions */
        if (stack.peek_call(0, Function::Identity::PackageFamily)) {
            return;
        }

        if (event_type == Event::Type::ClosureCallEntry) {
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            Environment* env = environment_table.lookup(event.get_rho());
            const Function* function = call->get_function();
            table_.record(depth_,
                          call->get_id(),
                          "ent",
                          function->get_qualified_name(),
                          env->get_id(),
                          env->get_receiver_eval_id(),
                          env->get_parent_eval_id(),
                          env->get_formatted_source());
            ++depth_;
        }

        else if (event_type == Event::Type::ClosureCallExit) {
            --depth_;
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            Environment* env = environment_table.lookup(event.get_rho());
            const Function* function = call->get_function();
            table_.record(depth_,
                          call->get_id(),
                          "ext",
                          function->get_qualified_name(),
                          env->get_id(),
                          env->get_receiver_eval_id(),
                          env->get_parent_eval_id(),
                          env->get_formatted_source());
        }

        else if (event_type == Event::Type::VariableDefinition ||
                 event_type == Event::Type::VariableAssignment) {
            SEXP r_rho = event.get_rho();
            std::string varname = CHAR(PRINTNAME(event.get_variable()));

            /* ignore *tmp* used by R internals for intermediate computation */
            if (is_tmp_val_symbol_(varname)) {
                return;
            }

            int eval_count = stack.count_call(Function::Identity::EvalFamily);

            /* if we are not inside eval, then exit */
            if (eval_count == 0) {
                return;
            }

            for (int i = 0; i < eval_count; ++i) {
                Call* eval_call =
                    stack.peek_call(i, Function::Identity::EvalFamily);
                Environment* environment = environment_table.lookup(r_rho);
                if (environment->get_parent_eval_id() < eval_call->get_id()) {
                    table_.record(depth_,
                                  NA_INTEGER,
                                  event.get_short_name(),
                                  varname,
                                  environment->get_id(),
                                  environment->get_receiver_eval_id(),
                                  environment->get_parent_eval_id(),
                                  environment->get_formatted_source());
                    return;
                }
            }
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
