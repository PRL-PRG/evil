#include "r_utilities.h"
#include "r_init.h"

const char* MissingStringValue = "_evil_missing_string_value_";

SEXP sexp_typeof(SEXP x) {
    return ScalarString(Rf_type2str(TYPEOF(x)));
}

SEXP mark_parsed_expression(SEXP x, SEXP parse_fun_name) {
    Rf_setAttrib(x, R_ParsedExpressionAttrib, parse_fun_name);
    return x;
}

/* NOTE: null_size field ensures that empty attributes can be given size 0 but a
 * literal NULL can be given size 1. The value has to be decided by the calling
 * context. */
int get_ast_size(SEXP ast, int null_size) {
    switch (TYPEOF(ast)) {
    case NILSXP:
        return null_size;
    case EXPRSXP: {
        int size = Rf_length(ast);
        int ast_size = 0;
        for (int index = 0; index < size; ++index) {
            ast_size += get_ast_size(VECTOR_ELT(ast, index), 1);
        }
        return ast_size + get_ast_size(Rf_getAttrib(ast, R_NamesSymbol), 0);
    }
    case LISTSXP:
    case LANGSXP: {
        int ast_size = 0;
        SEXP pointer = ast;
        while (pointer != R_NilValue) {
            ast_size +=
                get_ast_size(CAR(pointer), 1) + get_ast_size(TAG(pointer), 0);
            pointer = CDR(pointer);
        }
        return ast_size;
    }
    case SYMSXP:
        return strcmp(CHAR(PRINTNAME(ast)), "") != 0;

    default:
        return 1;
    }
}

SEXP r_get_ast_size(SEXP ast) {
    int size = get_ast_size(ast, 1);
    return ScalarInteger(size);
}

int get_sexp_type(SEXP r_value, int follow_symbol) {
    int value_type = TYPEOF(r_value);

    if (value_type == PROMSXP) {
        SEXP r_promval = dyntrace_get_promise_value(r_value);

        if (r_promval == R_UnboundValue) {
            SEXP r_promexpr = dyntrace_get_promise_expression(r_value);
            int expr_value_type = TYPEOF(r_promexpr);

            if (expr_value_type == SYMSXP && follow_symbol) {
                SEXP r_env = dyntrace_get_promise_environment(r_value);
                SEXP r_binding_value = Rf_findVarInFrame(r_env, r_promexpr);
                return get_sexp_type(r_binding_value, 1);
            }

            if (TYPEOF(r_promexpr) != LANGSXP) {
                return get_sexp_type(r_promexpr, 1);
            }
        } else {
            return get_sexp_type(r_promval, 0);
        }
    }

    return value_type;
}

