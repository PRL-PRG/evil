#ifndef EVIL_SIDE_EFFECT_ANALYSIS_H
#define EVIL_SIDE_EFFECT_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "WritesTable.h"
#include "ReadsTable.h"

class SideEffectAnalysis: public Analysis {
  public:
    SideEffectAnalysis()
        : Analysis(), writes_table_(WritesTable()), reads_table_(ReadsTable()) {
    }

    void analyze(TracerState& tracer_state, Event& event) override {
        Event::Type event_type = event.get_type();

        if (event_type != Event::Type::VariableAssignment &&
            event_type != Event::Type::VariableDefinition &&
            event_type != Event::Type::VariableLookup &&
            event_type != Event::Type::VariableRemoval) {
            return;
        }

        SEXP r_rho = event.get_rho();
        std::string envkind = tracer_state.get_envkind(r_rho);

        /* if envkind is uninitialized, it means this environment is being
         * prepared by the implementation.  */
        if (envkind == MissingStringValue) {
            return;
        }

        int eval_call_id = tracer_state.get_last_eval_call_id();
        SEXP r_variable = event.get_variable();
        const char* variable = CHAR(STRING_ELT(r_variable, 0));


        if (event_type == Event::Type::VariableLookup) {
            SEXP r_value = event.get_value();
            std::string valuetype = Rf_type2char(get_sexp_type(r_value, true));
            reads_table_.record(
                eval_call_id, true, false, envkind, variable, valuetype);
        } else {
            int nonlocal_count = 0;
            int eval_env_depth = NA_INTEGER;
            /* loop ignores first eval call because that is a dummy call
             * representing top-level  */
            for (int i = tracer_state.get_eval_call_count() - 1; i > 0; --i) {
                int call_id = tracer_state.get_eval_call_id(i);
                SEXP r_env = tracer_state.get_eval_env(i);

                /* we don't care about local writes */
                if (tracer_state.is_local_environment(r_rho, call_id)) {
                    break;
                }

                if (r_rho == r_env) {
                    eval_env_depth = tracer_state.get_eval_call_count() - 1 - i;
                }

                ++nonlocal_count;
            }

            if (nonlocal_count != 0) {
                writes_table_.record(eval_call_id,
                                     event_type_to_string(event_type),
                                     nonlocal_count - 1,
                                     variable,
                                     eval_env_depth,
                                     envkind);
            }
        }
    }

    std::vector<Table*> get_tables() override {
        return {&writes_table_, &reads_table_};
    }

  private:
    WritesTable writes_table_;
    ReadsTable reads_table_;
};

#endif /* EVIL_SIDE_EFFECT_ANALYSIS_H */
