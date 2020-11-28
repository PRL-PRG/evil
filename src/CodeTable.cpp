#include "CodeTable.h"
#include "r_utilities.h"

static SEXP r_name_ = R_NilValue;

SEXP CodeTable::get_name() {
    if(r_name_ == R_NilValue) {
        r_name_ = install("code");
    }
    return r_name_;
}

const char* get_package_name(SEXP r_call, SEXP r_rho) {
    /* if package is not provided, then we return null  */
    if (CADR(r_call) == R_MissingArg) {
        return nullptr;
    }

    bool character_only = asLogical(
        Rf_eval(Rf_findVarInFrame(r_rho, CharacterDotOnlySymbol), r_rho));

    SEXP r_package_name_promise = Rf_findVarInFrame(r_rho, PackageSymbol);

    /*  if character.only is true, then package is a symbol bound to a string */
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

    return "???";
}

void CodeTable::inspect_and_record(CodeTable* table,
                                 SEXP r_call,
                                 SEXP r_rho,
                                 int call_id) {
    if (is_call_to("library", r_call)) {
        const char* package_name = get_package_name(r_call, r_rho);
        table->record_call(call_id, "library", package_name == nullptr ?  "" : package_name);
    }
    else if (is_call_to("require", r_call)) {
        const char* package_name = get_package_name(r_call, r_rho);
        table->record_call(call_id, "require", package_name == nullptr ?  "" : package_name);
    }
}
