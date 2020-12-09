#ifndef EVIL_SIDE_EFFECT_ANALYSIS_H
#define EVIL_SIDE_EFFECT_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "SideEffectTable.h"

bool is_local_environment(int eval_call_id, SEXP r_rho);

class SideEffectAnalysis: public Analysis {
  public:
    SideEffectAnalysis(): Analysis(), side_effect_table_(SideEffectTable()) {
    }

    void analyze(CallState& call_state) override {
        Event event = call_state.get_event();

        if (event != Event::VariableAssignment &&
            event != Event::VariableDefinition &&
            event != Event::VariableLookup && event != Event::VariableRemoval) {
            return;
        }

        int eval_call_id = call_state.get_eval_call_id();
        SEXP r_variable = call_state.get_variable();
        SEXP r_rho = call_state.get_rho();
        int local = is_local_environment(eval_call_id, r_rho);
        std::string environment_class = get_environment_class_(r_rho);

        side_effect_table_.record(eval_call_id,
                                  event_to_string(event),
                                  CHAR(STRING_ELT(r_variable, 0)),
                                  local,
                                  environment_class);
    }

    std::vector<Table*> get_tables() override {
        return {&side_effect_table_};
    }

  private:
    SideEffectTable side_effect_table_;

    std::string get_environment_class_(SEXP r_rho) {
        if (r_rho == R_GlobalEnv) {
            return "global";
        }

        if (r_rho == R_BaseEnv || r_rho == R_BaseNamespace) {
            return "base";
        }

        if (R_IsPackageEnv(r_rho)) {
            SEXP r_name = R_PackageEnvName(r_rho);
            const char* name = CHAR(STRING_ELT(r_name, 0));
            return std::string(name + strlen("package:"));
        }

        return "<function>";
    }
};

#endif /* EVIL_SIDE_EFFECT_ANALYSIS_H */
