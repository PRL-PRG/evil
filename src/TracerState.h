#ifndef EVIL_TRACER_STATE_H
#define EVIL_TRACER_STATE_H

#include "r_utilities.h"
#include "Event.h"
#include <unordered_map>

class TracerState {
  private:
    struct env_info_t {
        int eval_call_id;
        std::string envkind;
    };

    struct eval_call_info_t {
        int call_id;
        SEXP r_env;
        int frame_depth;
    };

  public:
    TracerState() {
        add_environment_(R_GlobalEnv, "global", 0);
        add_environment_(R_BaseEnv, "package:base", 0);
        add_environment_(R_BaseNamespace, "package:base", 0);
    }

    int get_eval_call_id(int index) {
        return eval_calls_[index].call_id;
    }

    int get_last_eval_call_id() {
        return get_eval_call_id(get_eval_call_count() - 1);
    }

    SEXP get_eval_env(int index) {
        return eval_calls_[index].r_env;
    }

    SEXP get_last_eval_env() {
        return get_eval_env(get_eval_call_count() - 1);
    }

    int get_eval_frame_depth(int index) {
        return eval_calls_[index].frame_depth;
    }

    int get_last_eval_frame_depth() {
        return get_eval_frame_depth(get_eval_call_count() - 1);
    }

    int get_current_frame_depth() {
        return dyntrace_get_frame_depth();
    }

    int get_eval_call_count() {
        return eval_calls_.size();
    }

    bool is_local_environment(SEXP r_rho, int eval_call_id) {
        auto result = environments_.find(r_rho);
        /* NOTE: if environment is not present in the map, then it means that it
         * was created before all calls on the stack. Setting it to -1 will have
         * that effect.  */
        int environment_call_id =
            result != environments_.end() ? result->second.eval_call_id : -1;

        return environment_call_id >= eval_call_id;
    }

    std::string get_envkind(SEXP r_env) {
        auto result = add_environment_(r_env, MissingStringValue);

        env_info_t& env_info(result->second);

        if (env_info.envkind != MissingStringValue) {
            return env_info.envkind;
        }

        if (R_IsPackageEnv(r_env)) {
            SEXP r_name = R_PackageEnvName(r_env);
            env_info.envkind = CHAR(STRING_ELT(r_name, 0));
            return env_info.envkind;
        }

        return env_info.envkind;
    }

    void analyze(Event& event) {
        if (event.get_type() == Event::Type::ClosureCallEntry) {
            SEXP r_rho = event.get_rho();
            SEXP r_name = event.get_call();
            std::string name = TYPEOF(CAR(r_name)) == SYMSXP
                                   ? CHAR(PRINTNAME(CAR(r_name)))
                                   : "<object>";
            add_environment_(r_rho, std::string("function:") + name);
        }
        if (event.get_type() == Event::Type::ClosureCallExit) {
            if (event.is_call_to("new.env")) {
                SEXP r_result = event.get_result();
                set_envkind_(r_result, "explicit:new.env");
            } else if (event.is_call_to("list2env")) {
                SEXP r_result = event.get_result();
                set_envkind_(r_result, "explicit:list2env");
            }
        }
    }

    void push_eval_call(int call_id, SEXP r_env, int frame_depth) {
        eval_calls_.push_back({call_id, r_env, frame_depth});
    }

    void pop_eval_call() {
        eval_calls_.pop_back();
    }

  private:
    std::unordered_map<SEXP, env_info_t> environments_;

    std::vector<eval_call_info_t> eval_calls_;

    /* NOTE: this function serves the dual purpose of looking up or inserting
     * and looking up. For this, it leverages the fact that insert only inserts
     * if the key is not already in map. Otherwise, it returns an iterator to
     * the existing binding.   */
    std::unordered_map<SEXP, env_info_t>::iterator
    add_environment_(SEXP r_env,
                     const std::string& envkind,
                     int eval_call_id = -1) {
        eval_call_id = eval_call_id == -1 ? get_eval_call_id() : eval_call_id;
        env_info_t env_info{eval_call_id, envkind};
        auto result = environments_.insert({r_env, env_info});
        return result.first;
    }

    void set_envkind_(SEXP r_env, const std::string& envkind) {
        env_info_t env_info{get_eval_call_id(), envkind};
        auto result = environments_.insert({r_env, env_info});
        if (!result.second) {
            result.first->second.envkind = envkind;
        }
    }
};

#endif /* EVIL_CALL_STATE_H */
