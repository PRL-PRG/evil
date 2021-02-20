#ifndef EVIL_TRACER_STATE_H
#define EVIL_TRACER_STATE_H

#include "r_utilities.h"
#include "Event.h"
#include <unordered_map>
#include "Function.h"
#include "Stack.h"
#include "FunctionTable.h"

class TracerState {
  private:
    struct env_info_t {
        int eval_call_id;
        std::string envkind;
    };

  public:
    TracerState() {
        add_environment_(R_GlobalEnv, "global", 0);
        add_environment_(R_BaseEnv, "package:base", 0);
        add_environment_(R_BaseNamespace, "package:base", 0);
    }

    FunctionTable& get_function_table() {
        return function_table_;
    }

    Stack& get_stack() {
        return stack_;
    }

    const Stack& get_stack() const {
        return stack_;
    }

    int get_current_frame_depth() {
        return dyntrace_get_frame_depth();
    }

    int get_last_eval_call_id() {
        Stack& stack = get_stack();
        Call* call = stack.peek_call(0, Function::Type::Eval);
        if (call == nullptr) {
            return 0;
        }
        return call->get_id();
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

    const char* get_call_name(SEXP r_call) {
        return TYPEOF(CAR(r_call)) == SYMSXP ? CHAR(PRINTNAME(CAR(r_call)))
                                             : "<object>";
    }

    void analyze(Event& event) {
        Event::Type event_type = event.get_type();

        if (event_type == Event::Type::GcAllocation) {
            SEXP r_object = event.get_object();
            if (TYPEOF(r_object) == CLOSXP) {
                FunctionTable& function_table = get_function_table();

                function_table.insert(r_object);
            }
        }

        else if (event_type == Event::Type::GcUnmark) {
            /* NOTE: this causes deletion of Function objects which can still be
             * referenced by Call objects on stack. This leads to segfaults on
             * some program executions. */
            // SEXP r_object = event.get_object();
            // if (TYPEOF(r_object) == CLOSXP) {
            //    FunctionTable& function_table =
            //        tracer_state.get_function_table();
            //
            //    function_table.remove(r_object);
            //}
        }

        else if (event_type == Event::Type::ContextEntry) {
            void* call_context = event.get_call_context();
            StackFrame frame = StackFrame::from_context(call_context);
            Stack& stack = get_stack();
            stack.push(frame);
        }

        else if (event_type == Event::Type::ContextExit) {
            void* call_context = event.get_call_context();
            Stack& stack = get_stack();
            StackFrame frame = stack.pop();

            if (!frame.is_context()) {
                Rf_error("mismatched stack frame, expected context got call");
            } else if (frame.as_context() != call_context) {
                Rf_error("mismatched context on stack, expected %p got %p",
                         call_context,
                         frame.as_context());
            }
        }

        else if (event_type == Event::Type::EvalEntry) {
            Stack& stack = get_stack();
            Call* call = stack.peek_call(0, Function::Type::Eval);
            if (call != nullptr) {
                call->increment_interp_eval_count();
            }
        }

        else if (event_type == Event::Type::ClosureCallEntry) {
            SEXP r_call = event.get_call();
            SEXP r_op = event.get_op();
            SEXP r_args = event.get_args();
            SEXP r_rho = event.get_rho();

            const char* name = get_call_name(r_call);
            add_environment_(r_rho, std::string("function:") + name);

            Stack& stack = get_stack();
            Function* function = get_function_table().lookup(r_op);

            StackFrame frame = StackFrame::from_call(
                new Call(function, r_call, r_args, r_rho, stack.size()));

            stack.push(frame);

        }

        else if (event_type == Event::Type::ClosureCallExit) {
            SEXP r_call = event.get_call();
            SEXP r_op = event.get_op();
            SEXP r_args = event.get_args();
            SEXP r_rho = event.get_rho();
            SEXP r_result = event.get_result();

            if (event.is_call_to("new.env")) {
                set_envkind_(r_result, "explicit:new.env");
            } else if (event.is_call_to("list2env")) {
                set_envkind_(r_result, "explicit:list2env");
            }

            Stack& stack = get_stack();
            StackFrame frame = stack.pop();
            Call* call = nullptr;

            if (!frame.is_call()) {
                Rf_error("mismatched stack frame, expected call got context");
            } else {
                call = frame.as_call();
                if (call->get_expression() != r_call ||
                    call->get_arguments() != r_args ||
                    call->get_environment() != r_rho) {
                    Rf_error("mismatched call on stack");
                }
            }

            if (call->get_function()->has_type(Function::Type::Eval)) {
                interp_eval_counts_.push_back(call->get_interp_eval_count());
            }

            delete call;
        }
    }

    void set_eval_call_info(int call_id, SEXP r_env, int frame_depth) {
        Stack& stack = get_stack();
        Call* call = stack.peek_call(0, Function::Type::Eval);
        if (call == nullptr) {
            Rf_error("set_eval_call_info: expected eval call on the stack");
        }
        call->set_id(call_id);
        call->set_eval_environment(r_env);
    }

    int pop_interp_eval_count() {
        int count = interp_eval_counts_.back();
        interp_eval_counts_.pop_back();
        return count;
    }

  private:
    FunctionTable function_table_;

    Stack stack_;

    std::vector<int> interp_eval_counts_;

    std::unordered_map<SEXP, env_info_t> environments_;

    /* NOTE: this function serves the dual purpose of looking up or inserting
     * and looking up. For this, it leverages the fact that insert only inserts
     * if the key is not already in map. Otherwise, it returns an iterator to
     * the existing binding.   */
    std::unordered_map<SEXP, env_info_t>::iterator
    add_environment_(SEXP r_env,
                     const std::string& envkind,
                     int eval_call_id = -1) {
        eval_call_id =
            eval_call_id == -1 ? get_last_eval_call_id() : eval_call_id;

        env_info_t env_info{eval_call_id, envkind};
        auto result = environments_.insert({r_env, env_info});
        if (result.first->second.envkind == MissingStringValue) {
            result.first->second.envkind = envkind;
        }
        return result.first;
    }

    void set_envkind_(SEXP r_env, const std::string& envkind) {
        env_info_t env_info{get_last_eval_call_id(), envkind};
        auto result = environments_.insert({r_env, env_info});
        if (!result.second) {
            result.first->second.envkind = envkind;
        }
    }
};

#endif /* EVIL_CALL_STATE_H */
