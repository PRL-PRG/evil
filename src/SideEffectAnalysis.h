#ifndef EVIL_SIDE_EFFECT_ANALYSIS_H
#define EVIL_SIDE_EFFECT_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "SideEffectTable.h"
#include "LookupTable.h"

class SideEffectAnalysis: public Analysis {
  public:
    SideEffectAnalysis()
        : Analysis()
        , side_effect_table_(SideEffectTable())
        , lookup_table_(LookupTable()) {
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

        int eval_call_id = tracer_state.get_eval_call_id();
        SEXP r_variable = event.get_variable();
        const char* variable = CHAR(STRING_ELT(r_variable, 0));

        int local = tracer_state.is_local_environment(r_rho);

        if (event_type == Event::Type::VariableLookup) {
            std::string valuetype = Rf_type2char(TYPEOF(event.get_value()));
            lookup_table_.record(
                eval_call_id, true, local, envkind, variable, valuetype);
        } else {
            side_effect_table_.record(eval_call_id,
                                      event_type_to_string(event_type),
                                      variable,
                                      local,
                                      envkind);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&side_effect_table_, &lookup_table_};
    }

  private:
    SideEffectTable side_effect_table_;
    LookupTable lookup_table_;
};

#endif /* EVIL_SIDE_EFFECT_ANALYSIS_H */
