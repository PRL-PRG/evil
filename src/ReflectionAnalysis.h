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

        if (event.is_call_to("sys.call")) {
            int which = event.get_integer_argument(WhichSymbol);
            int accessed_frame_depth = compute_which_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, which);
            int external =
                is_external_access(eval_frame_depth, accessed_frame_depth);
            reflection_table_.record(eval_call_id,
                                     "sys.call",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     accessed_frame_depth,
                                     external);
        }

        else if (event.is_call_to("sys.frame")) {
            int which = event.get_integer_argument(WhichSymbol);
            int accessed_frame_depth = compute_which_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, which);
            int external =
                is_external_access(eval_frame_depth, accessed_frame_depth);
            reflection_table_.record(eval_call_id,
                                     "sys.frame",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     accessed_frame_depth,
                                     external);
        }

        else if (event.is_call_to("sys.function")) {
            int which = event.get_integer_argument(WhichSymbol);
            int accessed_frame_depth = compute_which_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, which);
            int external =
                is_external_access(eval_frame_depth, accessed_frame_depth);
            reflection_table_.record(eval_call_id,
                                     "sys.function",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     accessed_frame_depth,
                                     external);
        }

        else if (event.is_call_to("sys.parent")) {
            int n = event.get_integer_argument(NSymbol);
            int accessed_frame_depth = compute_n_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, n);
            int external =
                is_external_access(eval_frame_depth, accessed_frame_depth);
            reflection_table_.record(eval_call_id,
                                     "sys.parent",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     accessed_frame_depth,
                                     external);
        }

        else if (event.is_call_to("sys.calls")) {
            reflection_table_.record(eval_call_id,
                                     "sys.calls",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     NA_INTEGER,
                                     1);
        }

        else if (event.is_call_to("sys.frames")) {
            reflection_table_.record(eval_call_id,
                                     "sys.frames",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     NA_INTEGER,
                                     1);
        }

        else if (event.is_call_to("sys.parents")) {
            reflection_table_.record(eval_call_id,
                                     "sys.parents",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     NA_INTEGER,
                                     1);
        }

        else if (event.is_call_to("parent.frame")) {
            int n = event.get_integer_argument(NSymbol);
            int accessed_frame_depth = compute_n_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, n);
            int external =
                is_external_access(eval_frame_depth, accessed_frame_depth);
            reflection_table_.record(eval_call_id,
                                     "parent.frame",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     accessed_frame_depth,
                                     external);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&reflection_table_};
    }

  private:
    ReflectionTable reflection_table_;

    int compute_n_accessed_frame_depth(int eval_frame_depth,
                                       int current_frame_depth,
                                       int n) {
        return current_frame_depth - n;
    }

    int compute_which_accessed_frame_depth(int eval_frame_depth,
                                           int current_frame_depth,
                                           int which) {
        return (which >= 0) ? which : current_frame_depth - which;
    }

    bool is_external_access(int eval_frame_depth, int accessed_frame_depth) {
        return accessed_frame_depth <= eval_frame_depth;
    }
};

#endif /* EVIL_REFLECTION_ANALYSIS_H */
