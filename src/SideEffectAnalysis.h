#ifndef EVIL_SIDE_EFFECT_ANALYSIS_H
#define EVIL_SIDE_EFFECT_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"

bool is_local_environment(int eval_call_id, SEXP r_rho);

class SideEffectAnalysis: public Analysis {
  public:
    SideEffectAnalysis(): Analysis() {
    }

    void
    record_call(int eval_call_id,
                const std::string& category,
                const char* variable,
                int local = NA_LOGICAL,
                const std::string& environment_class = MissingStringValue) {
        eval_call_ids_.push_back(eval_call_id);
        category_.push_back(category);
        variable_.push_back(variable);
        local_.push_back(local);
        environment_class_.push_back(environment_class);
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

        record_call(eval_call_id,
                    event_to_string(event),
                    CHAR(STRING_ELT(r_variable, 0)),
                    local,
                    environment_class);
    }

    std::vector<table_t> get_tables() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_call_ids_))},
             {"category", PROTECT(create_character_vector(category_))},
             {"variable", PROTECT(create_character_vector(variable_))},
             {"local", PROTECT(create_logical_vector(local_))},
             {"environment_class",
              PROTECT(create_character_vector(environment_class_))}});

        UNPROTECT(5);

        return {{"side_effect", r_data_frame}};
    }

  private:
    std::vector<int> eval_call_ids_;
    std::vector<std::string> category_;
    std::vector<std::string> variable_;
    std::vector<int> local_;
    std::vector<std::string> environment_class_;

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
