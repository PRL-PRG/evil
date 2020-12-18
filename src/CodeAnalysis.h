#ifndef EVIL_CODE_ANALYSIS_H
#define EVIL_CODE_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "CodeTable.h"

class CodeAnalysis: public Analysis {
  public:
    CodeAnalysis(): Analysis(), code_table_(CodeTable()) {
    }

    void analyze(TracerState& tracer_state, Event& event) override final {
        if (event.get_type() != Event::Type::ClosureCallEntry) {
            return;
        }

        SEXP r_call = event.get_call();
        SEXP r_rho = event.get_rho();
        int eval_call_id = tracer_state.get_last_eval_call_id();

        if (event.is_call_to("library")) {
            std::string package_name = get_package_name_(r_call, r_rho);
            code_table_.record(eval_call_id, "library", package_name);
        } else if (event.is_call_to("require")) {
            std::string package_name = get_package_name_(r_call, r_rho);
            code_table_.record(eval_call_id, "require", package_name);
        } else if (event.is_call_to("source")) {
            std::string file_path = event.get_character_argument(FileSymbol);
            int local = event.get_logical_argument(LocalSymbol);
            code_table_.record(eval_call_id, "source", file_path, local);
        } else if (event.is_call_to("sys.source")) {
            std::string file_path = event.get_character_argument(FileSymbol);
            code_table_.record(eval_call_id, "sys.source", file_path);
        }
    }

    std::vector<Table*> get_tables() override {
        return {&code_table_};
    }

  private:
    CodeTable code_table_;

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

#endif /* EVIL_CODE_ANALYSIS_H */
