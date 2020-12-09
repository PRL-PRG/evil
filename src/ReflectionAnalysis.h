#ifndef EVIL_REFLECTION_ANALYSIS_H
#define EVIL_REFLECTION_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"

class ReflectionAnalysis: public Analysis {
  public:
    ReflectionAnalysis(): Analysis() {
    }

    void record_call(int eval_call_id,
                     const char* function,
                     int eval_frame_depth = NA_INTEGER,
                     int call_frame_depth = NA_INTEGER,
                     int reverse_frame_depth = NA_INTEGER) {
        eval_call_ids_.push_back(eval_call_id);
        functions_.push_back(function);
        eval_frame_depths_.push_back(eval_frame_depth);
        call_frame_depths_.push_back(call_frame_depth);
        int accessed_frame_depth = reverse_frame_depth == NA_INTEGER
                                       ? NA_INTEGER
                                       : call_frame_depth - reverse_frame_depth;
        accessed_frame_depths_.push_back(accessed_frame_depth);
        int leak = NA_LOGICAL;
        if (eval_frame_depth != NA_INTEGER && call_frame_depth != NA_INTEGER &&
            accessed_frame_depth != NA_INTEGER) {
            leak = accessed_frame_depth <= eval_frame_depth;
        }
        leaks_.push_back(leak);
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
            record_call(eval_call_id, "sys.calls");
        } else if (call_state.is_call_to("sys.frames")) {
            record_call(eval_call_id, "sys.frames");
        } else if (call_state.is_call_to("sys.parents")) {
            record_call(eval_call_id, "sys.parents");
        } else if (call_state.is_call_to("sys.frame")) {
            int reverse_frame_index =
                call_state.get_integer_argument(WhichSymbol);
            record_call(eval_call_id,
                        "sys.frame",
                        eval_frame_depth,
                        current_frame_depth,
                        reverse_frame_index);
        } else if (call_state.is_call_to("sys.call")) {
            int reverse_frame_index =
                call_state.get_integer_argument(WhichSymbol);
            record_call(eval_call_id,
                        "sys.call",
                        eval_frame_depth,
                        current_frame_depth,
                        reverse_frame_index);
        } else if (call_state.is_call_to("sys.function")) {
            int reverse_frame_index =
                call_state.get_integer_argument(WhichSymbol);
            record_call(eval_call_id,
                        "sys.function",
                        eval_frame_depth,
                        current_frame_depth,
                        reverse_frame_index);
        } else if (call_state.is_call_to("parent.frame")) {
            int reverse_frame_index = call_state.get_integer_argument(NSymbol);
            record_call(eval_call_id,
                        "parent.frame",
                        eval_frame_depth,
                        current_frame_depth,
                        reverse_frame_index);
        }
    }

    std::vector<table_t> get_tables() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_call_ids_))},
             {"function", PROTECT(create_character_vector(functions_))},
             {"eval_frame_depth",
              PROTECT(create_integer_vector(eval_frame_depths_))},
             {"call_frame_depth",
              PROTECT(create_integer_vector(call_frame_depths_))},
             {"accessed_frame_depth",
              PROTECT(create_integer_vector(accessed_frame_depths_))},
             {"leak", PROTECT(create_logical_vector(leaks_))}});

        UNPROTECT(6);

        return {{"reflection", r_data_frame}};
    }

  private:
    std::vector<int> eval_call_ids_;
    std::vector<std::string> functions_;
    std::vector<int> eval_frame_depths_;
    std::vector<int> call_frame_depths_;
    std::vector<int> accessed_frame_depths_;
    std::vector<int> leaks_;
};

#endif /* EVIL_REFLECTION_ANALYSIS_H */
