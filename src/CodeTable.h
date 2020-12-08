#ifndef EVIL_CODE_TABLE_H
#define EVIL_CODE_TABLE_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Table.h"

class CodeTable: public Table {
  public:
    CodeTable(): Table() {
    }

    void record_call(int call_id,
                     const std::string& function,
                     const std::string& description,
                     int local = 0) {
        call_ids_.push_back(call_id);
        functions_.push_back(function);
        descriptions_.push_back(description);
        locals_.push_back(local);
    }

    void inspect_and_record(CallState& call_state) override final {
        if (call_state.get_event() != Event::ClosureCallEntry) {
            return;
        }

        SEXP r_call = call_state.get_call();
        SEXP r_rho = call_state.get_rho();
        int eval_call_id = call_state.get_eval_call_id();

        if (call_state.is_call_to("library")) {
            std::string package_name = get_package_name_(r_call, r_rho);
            record_call(eval_call_id, "library", package_name);
        } else if (call_state.is_call_to("require")) {
            std::string package_name = get_package_name_(r_call, r_rho);
            record_call(eval_call_id, "require", package_name);
        } else if (call_state.is_call_to("source")) {
            std::string file_path =
                call_state.get_character_argument(FileSymbol);
            int local = call_state.get_logical_argument(LocalSymbol);
            record_call(eval_call_id, "source", file_path, local);
        } else if (call_state.is_call_to("sys.source")) {
            std::string file_path =
                call_state.get_character_argument(FileSymbol);
            record_call(eval_call_id, "sys.source", file_path);
        }
    }

    SEXP to_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"call_id", PROTECT(create_integer_vector(call_ids_))},
             {"function", PROTECT(create_character_vector(functions_))},
             {"description", PROTECT(create_character_vector(descriptions_))},
             {"local", PROTECT(create_logical_vector(locals_))}});

        UNPROTECT(4);

        return r_data_frame;
    }

    static SEXP get_name();

  private:
    std::vector<int> call_ids_;
    std::vector<std::string> functions_;
    std::vector<std::string> descriptions_;
    std::vector<int> locals_;

    std::string get_package_name_(SEXP r_call, SEXP r_rho) {
        /* if package is not provided, then we return null  */
        if (CADR(r_call) == R_MissingArg) {
            return MissingStringValue;
        }

        bool character_only = asLogical(
            Rf_eval(Rf_findVarInFrame(r_rho, CharacterDotOnlySymbol), r_rho));

        SEXP r_package_name_promise = Rf_findVarInFrame(r_rho, PackageSymbol);

        /*  if character.only is true, then package is a symbol bound to a
         * string */
        if (character_only) {
            SEXP r_package_name = Rf_eval(r_package_name_promise, r_rho);
            if (TYPEOF(r_package_name) == STRSXP &&
                STRING_ELT(r_package_name, 0) != NA_STRING) {
                return CHAR(STRING_ELT(r_package_name, 0));
            }
        } else {
            SEXP r_package_name =
                dyntrace_get_promise_expression(r_package_name_promise);
            if (TYPEOF(r_package_name) == SYMSXP) {
                return CHAR(PRINTNAME(r_package_name));
            } else if (TYPEOF(r_package_name) == STRSXP &&
                       STRING_ELT(r_package_name, 0) != NA_STRING) {
                return CHAR(STRING_ELT(r_package_name, 0));
            }
        }

        return MissingStringValue;
    }
};

#endif /* EVIL_CODE_TABLE_H */
