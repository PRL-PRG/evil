#ifndef EVIL_EXECUTION_TRACE_ANALYSIS_H
#define EVIL_EXECUTION_TRACE_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "ExecutionTraceTable.h"
#include "WritesTable.h"

class ExecutionTraceAnalysis: public Analysis {
  public:
    ExecutionTraceAnalysis(): Analysis(), depth_(0) {
    }

    void analyze(TracerState& tracer_state, Event& event) override {
        Event::Type event_type = event.get_type();
        Stack& stack = tracer_state.get_stack();
        EnvironmentTable& environment_table =
            tracer_state.get_environment_table();

        /* ignore side effects happening in package functions */
        if (stack.peek_call(0, Function::Identity::PackageFamily)) {
            return;
        }

        if (event_type == Event::Type::ClosureCallEntry) {
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            Environment* env = environment_table.lookup(event.get_rho());
            const Function* function = call->get_function();
            // trace_table_.record(depth_,
            //                     call->get_id(),
            //                     "ent",
            //                     function->get_qualified_name(),
            //                     env->get_id(),
            //                     env->get_receiver_eval_id(),
            //                     env->get_parent_eval_id(),
            //                     env->get_formatted_source());
            ++depth_;
        }

        else if (event_type == Event::Type::ClosureCallExit) {
            --depth_;
            const StackFrame& frame = stack.peek();
            const Call* call = frame.as_call();
            Environment* env = environment_table.lookup(event.get_rho());
            const Function* function = call->get_function();
            // trace_table_.record(depth_,
            //                     call->get_id(),
            //                     "ext",
            //                     function->get_qualified_name(),
            //                     env->get_id(),
            //                     env->get_receiver_eval_id(),
            //                     env->get_parent_eval_id(),
            //                     env->get_formatted_source());
        }

        else if (event_type == Event::Type::VariableDefinition ||
                 event_type == Event::Type::VariableAssignment ||
                 event_type == Event::Type::VariableRemoval) {
            SEXP r_rho = event.get_rho();
            std::string varname = CHAR(PRINTNAME(event.get_variable()));
            int vartype = TYPEOF(event.get_value());

            /* ignore *tmp* used by R internals for intermediate computation */
            if (is_tmp_val_symbol_(varname)) {
                return;
            }

            int eval_count = stack.count_call(Function::Identity::EvalFamily);

            /* if we are not inside eval, then exit */
            if (eval_count == 0) {
                return;
            }

            Environment* environment = environment_table.lookup(r_rho);
            int parent_eval_id = environment->get_parent_eval_id();
            bool transitive = false;

            for (int i = 0; i < eval_count; ++i) {
                Call* eval_call =
                    stack.peek_call(i, Function::Identity::EvalFamily);

                bool in_envir = eval_call->get_eval_environment() == r_rho;

                if (parent_eval_id < eval_call->get_id()) {
                    int env_depth = compute_env_depth(stack, r_rho);
                    std::string env_class = environment->get_formatted_source();
                    if (environment->get_source() == Environment::Source::Unknown) {
                        SEXP parent_env;
                        PROTECT(parent_env = ENCLOS(r_rho));
                        if (is_S4_env(r_rho)) {
                            env_class += "S4";
                        } else if (is_R6_env(r_rho)) {
                            env_class = compute_R6_env_name(r_rho);
                        } else if (r_rho == userHooksEnv_) {
                            env_class = ".userHooksEnv";
                        } else if (parent_env == R_EmptyEnv) {
                            env_class += "Empty";
                        } else {
                            env_class += environment_name(parent_env);
                        }
                        UNPROTECT(1);
                    }

                    writes_table_.record(eval_call->get_id(),
                                         event.get_short_name(),
                                         transitive,
                                         varname,
                                         vartype,
                                         environment->get_id(),
                                         environment->get_parent_eval_id(),
                                         environment->get_receiver_eval_id(),
                                         env_class,
                                         env_depth,
                                         in_envir);

                    // /* output the trace first time */
                    // if (i == 0) {
                    //     trace_table_.record(
                    //         depth_,
                    //         NA_INTEGER,
                    //         event.get_short_name(),
                    //         varname,
                    //         environment->get_id(),
                    //         environment->get_receiver_eval_id(),
                    //         environment->get_parent_eval_id(),
                    //         environment->get_formatted_source());
                    // }

                } else {
                    break;
                }

                transitive = true;
            }
        }
    }

    std::vector<Table*> get_tables() override {
        // return {&trace_table_, &writes_table_};
        return {&writes_table_};
    }

  private:
    // ExecutionTraceTable trace_table_;
    WritesTable writes_table_;
    int depth_;

    bool is_tmp_val_symbol_(const std::string& name) {
        return name == "*tmp*";
    }

    SEXP userHooksEnv_ = Rf_eval(lang3(install("get"), mkString(".userHooksEnv"), R_GlobalEnv), R_GlobalEnv);

    std::string compute_R6_env_name(SEXP rho) {
        SEXP attr;
        PROTECT(attr = Rf_getAttrib(rho, install("class")));
        std::string acc = "";

        if (TYPEOF(attr) == STRSXP) {
            acc = "R6:";
            for (int i=0; i<XLENGTH(attr); i++) {
                acc += CHAR(STRING_ELT(attr, i));
                if (i < XLENGTH(attr) - 1) {
                    acc += ",";
                }
            }
        }
        UNPROTECT(1);
        return acc;
    }

    bool is_R6_env(SEXP rho) {
        SEXP attr;
        bool res;
        PROTECT(attr = Rf_getAttrib(rho, install("class")));
        res = TYPEOF(attr) == STRSXP && XLENGTH(attr) > 0;
        UNPROTECT(1);
        return res;
    }

    bool is_S4_env(SEXP rho) {
        SEXP parent;
        bool res = false;

        PROTECT(parent = ENCLOS(rho));
        if (parent != R_NilValue) {
            res = Rf_findVarInFrame3(parent, install(".MTable"), FALSE) != R_UnboundValue;
            res = res || Rf_findVarInFrame3(parent, install(".AllMTable"), FALSE) != R_UnboundValue;
        }
        UNPROTECT(1);
        return res;
    }

    int compute_env_depth(Stack &stack, SEXP r_rho) {
        // so the caller is 0
        int depth = -1;

        for (int n = 1; n < stack.size(); ++n) {
            Call *call = stack.peek_call(n);
            if (call != nullptr) {
                ++depth;
                SEXP call_env = call->get_environment();
                if (call_env == r_rho) {
                    return depth;
                }
            }
        }

        return NA_INTEGER;
    }

    std::string classify_environment(Call* eval_call, SEXP r_rho) {
        SEXP expr, res;
        std::string env_class = "???";

        Rprintf("C: %d, %p, %p\n", eval_call->get_depth(), eval_call->get_environment(), r_rho);
        PROTECT(expr = lang4(lang3(install("::"),
                                   install("evil"),
                                   install("classify_environment")),
                             ScalarInteger(eval_call->get_depth()),
                             eval_call->get_environment(),
                             r_rho));
        int error;
        PROTECT(res = R_tryEval(expr, eval_call->get_environment(), &error));
        if (!error) {
            env_class = CHAR(STRING_ELT(res, 0));
        }
        UNPROTECT(2);

        return env_class;
    }

    std::string environment_name(SEXP env) {
        if (R_IsPackageEnv(env) == TRUE) {
            // cf. builtin.c:432 do_envirName
            return CHAR(STRING_ELT(R_PackageEnvName(env), 0));
        } else if (R_IsNamespaceEnv(env) == TRUE) {
            // cf. builtin.c:434 do_envirName
            return CHAR(STRING_ELT(R_NamespaceEnvSpec(env), 0));
        } else {
            return "";
        }
    }
};

#endif /* EVIL_EXECUTION_TRACE_ANALYSIS_H */
