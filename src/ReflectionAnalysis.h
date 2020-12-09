#ifndef EVIL_REFLECTION_ANALYSIS_H
#define EVIL_REFLECTION_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "ReflectionTable.h"

class ReflectionAnalysis: public Analysis {
  public:
    ReflectionAnalysis(): Analysis(), reflection_table_(ReflectionTable()) {
    }

    void analyze(CallState& call_state) override {
        if (call_state.get_event() != Event::ClosureCallEntry) {
            return;
        }
        SEXP r_call = call_state.get_call();
        SEXP r_rho = call_state.get_rho();
        int eval_call_id = call_state.get_eval_call_id();
        int eval_frame_depth = call_state.get_eval_frame_depth();
        int current_frame_depth = call_state.get_current_frame_depth();

        if (call_state.is_call_to("sys.calls")) {
            reflection_table_.record(eval_call_id, "sys.calls");
        } else if (call_state.is_call_to("sys.frames")) {
            reflection_table_.record(eval_call_id, "sys.frames");
        } else if (call_state.is_call_to("sys.parents")) {
            reflection_table_.record(eval_call_id, "sys.parents");
        } else if (call_state.is_call_to("sys.frame")) {
            int reverse_frame_index =
                call_state.get_integer_argument(WhichSymbol);
            reflection_table_.record(eval_call_id,
                                     "sys.frame",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     reverse_frame_index);
        } else if (call_state.is_call_to("sys.call")) {
            int reverse_frame_index =
                call_state.get_integer_argument(WhichSymbol);
            reflection_table_.record(eval_call_id,
                                     "sys.call",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     reverse_frame_index);
        } else if (call_state.is_call_to("sys.function")) {
            int reverse_frame_index =
                call_state.get_integer_argument(WhichSymbol);
            reflection_table_.record(eval_call_id,
                                     "sys.function",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     reverse_frame_index);
        } else if (call_state.is_call_to("parent.frame")) {
            int reverse_frame_index = call_state.get_integer_argument(NSymbol);
            reflection_table_.record(eval_call_id,
                                     "parent.frame",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     reverse_frame_index);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&reflection_table_};
    }

  private:
    ReflectionTable reflection_table_;
};

#endif /* EVIL_REFLECTION_ANALYSIS_H */
