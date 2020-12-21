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

    void analyze(TracerState& tracer_state, Event& event) override {
        if (event.get_type() != Event::Type::ClosureCallEntry) {
            return;
        }
        SEXP r_call = event.get_call();
        SEXP r_rho = event.get_rho();
        int eval_call_id = tracer_state.get_last_eval_call_id();
        int eval_frame_depth = tracer_state.get_last_eval_frame_depth();
        int current_frame_depth = tracer_state.get_current_frame_depth();

        if (event.is_call_to("sys.calls")) {
            reflection_table_.record(eval_call_id, "sys.calls");
        } else if (event.is_call_to("sys.frames")) {
            reflection_table_.record(eval_call_id, "sys.frames");
        } else if (event.is_call_to("sys.parents")) {
            reflection_table_.record(eval_call_id, "sys.parents");
        } else if (event.is_call_to("sys.frame")) {
            int reverse_frame_index = event.get_integer_argument(WhichSymbol);
            int leaks = is_leaky_which(eval_frame_depth, current_frame_depth, which);
            reflection_table_.record(eval_call_id,
                                     "sys.frame",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     which = which,
                                     n = NA_INTEGER,
                                     leaks = leaks);
        } else if (event.is_call_to("sys.call")) {
            int reverse_frame_index = event.get_integer_argument(WhichSymbol);
            int leaks =
                is_leaky_which(eval_frame_depth, current_frame_depth, which);
            reflection_table_.record(eval_call_id,
                                     "sys.call",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     which = which,
                                     n = NA_INTEGER,
                                     leaks = leaks);
        } else if (event.is_call_to("sys.function")) {
            int reverse_frame_index = event.get_integer_argument(WhichSymbol);
            int leaks =
                is_leaky_which(eval_frame_depth, current_frame_depth, which);
            reflection_table_.record(eval_call_id,
                                     "sys.function",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     which = which,
                                     n = NA_INTEGER,
                                     leaks = leaks);
        } else if (event.is_call_to("parent.frame")) {
            int reverse_frame_index = event.get_integer_argument(NSymbol);
            int leaks =
                is_leaky_n(eval_frame_depth, current_frame_depth, n);
            reflection_table_.record(eval_call_id,
                                     "parent.frame",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     which = which,
                                     n = NA_INTEGER,
                                     leaks = leaks);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&reflection_table_};
    }

  private:
    ReflectionTable reflection_table_;
};

#endif /* EVIL_REFLECTION_ANALYSIS_H */
