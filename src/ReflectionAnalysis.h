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

        Stack& stack = tracer_state.get_stack();
        Call* call = stack.peek_call(0, Function::Identity::Eval);

        int current_frame_depth = stack.size();
        int eval_call_id = 0;
        int eval_frame_depth = 0;

        if (call != nullptr) {
            eval_call_id = call->get_id();
            eval_frame_depth = call->get_depth();
        }

        // NOTE: pos.to.env and as.environment need not be handled
        // because they look up the search path.
        // They cannot reflectively access environments
        // from the call stack frames before eval.
        /*
        if (event.is_call_to("pos.to.env")) {
            SEXP r_x = event.get_argument("x");
            int x = -1;
            if (TYPEOF(r_x) == INTSXP) {
                x = INTEGER(x)[0];
            } else if (TYPEOF(r_x) == REALSXP) {
                x = (int) (REAL(x)[0]);
            }
            // x = -1 means current environment is being accessed
            if (x != -1) {
                record_if_external(eval_call_id,
                                   "pos.to.env",
                                   eval_frame_depth,
                                   current_frame_depth,
                                   NA_INTEGER);
            }
        }

        if (event.is_call_to("as.environment")) {
            SEXP r_x = event.get_argument("x");
            int x = -1;
            if (TYPEOF(r_x) == INTSXP) {
                x = INTEGER(r_x)[0];
            } else if (TYPEOF(r_x) == REALSXP) {
                x = REAL(r_x)[0];
            } else if (TYPEOF(r_x) == STRSXP) {
                x = NA_INTEGER;
            }

            int external = 0;
            if (x == NA_INTEGER) {
                external = 1;
            } else if (x == -1) {
                external = 0;
            } else {
                external = is_external_access(eval_frame_depth, x);
            }

            record_if_external(eval_call_id,
                               "as.environment",
                               eval_frame_depth,
                               current_frame_depth,
                               x,
                               external);
        }
    }
    */

        if (event.is_call_to("sys.call")) {
            int which = event.get_integer_argument(WhichSymbol);
            int accessed_frame_depth = compute_which_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, which);

            record_if_external(eval_call_id,
                               "sys.call",
                               eval_frame_depth,
                               current_frame_depth,
                               accessed_frame_depth);
        }

        else if (event.is_call_to("sys.frame")) {
            int which = event.get_integer_argument(WhichSymbol);
            int accessed_frame_depth = compute_which_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, which);

            record_if_external(eval_call_id,
                               "sys.frame",
                               eval_frame_depth,
                               current_frame_depth,
                               accessed_frame_depth);
        }

        else if (event.is_call_to("sys.function")) {
            int which = event.get_integer_argument(WhichSymbol);
            int accessed_frame_depth = compute_which_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, which);

            record_if_external(eval_call_id,
                               "sys.function",
                               eval_frame_depth,
                               current_frame_depth,
                               accessed_frame_depth);
        }

        else if (event.is_call_to("sys.parent")) {
            int n = event.get_integer_argument(NSymbol);
            int accessed_frame_depth = compute_n_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, n);

            record_if_external(eval_call_id,
                               "sys.parent",
                               eval_frame_depth,
                               current_frame_depth,
                               accessed_frame_depth);
        }

        else if (event.is_call_to("sys.calls")) {
            record_if_external(eval_call_id,
                               "sys.calls",
                               eval_frame_depth,
                               current_frame_depth,
                               NA_INTEGER);
        }

        else if (event.is_call_to("sys.frames")) {
            record_if_external(eval_call_id,
                               "sys.frames",
                               eval_frame_depth,
                               current_frame_depth,
                               NA_INTEGER);
        }

        else if (event.is_call_to("sys.parents")) {
            record_if_external(eval_call_id,
                               "sys.parents",
                               eval_frame_depth,
                               current_frame_depth,
                               NA_INTEGER);
        }

        else if (event.is_call_to("parent.frame")) {
            int n = event.get_integer_argument(NSymbol);
            int accessed_frame_depth = compute_n_accessed_frame_depth(
                eval_frame_depth, current_frame_depth, n);

            record_if_external(eval_call_id,
                               "parent.frame",
                               eval_frame_depth,
                               current_frame_depth,
                               accessed_frame_depth);
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

    void record_if_external(int eval_call_id,
                            const char* function,
                            int eval_frame_depth,
                            int current_frame_depth,
                            int accessed_frame_depth) {
        int external =
            accessed_frame_depth == NA_INTEGER
                ? 1
                : is_external_access(eval_frame_depth, accessed_frame_depth);

        if (external) {
            reflection_table_.record(eval_call_id,
                                     "sys.function",
                                     eval_frame_depth,
                                     current_frame_depth,
                                     accessed_frame_depth);
        }
    }
};

#endif /* EVIL_REFLECTION_ANALYSIS_H */
