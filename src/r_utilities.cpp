#include "r_utilities.h"
#include "r_init.h"

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

SEXP r_get_argument(SEXP r_call, SEXP r_rho, SEXP r_argument_name, int evaluate) {
    SEXP r_value = Rf_findVarInFrame(r_rho, r_argument_name);

    if(r_value == R_MissingArg || TYPEOF(r_value) != PROMSXP) {
        return r_value;
    } else if (evaluate) {
        return Rf_eval(r_value, r_rho);
    } else {
        return r_value;
    }
}

int get_argument_as_integer(SEXP r_call, SEXP r_rho, SEXP r_argument_name)  {
    SEXP r_value = r_get_argument(r_call, r_rho, r_argument_name, 1);
    int result = NA_INTEGER;

    if(TYPEOF(r_value) == REALSXP) {
        double value = REAL(r_value)[0];
        result = value == NA_REAL ? NA_INTEGER : (int)(value);
    }
    else if(TYPEOF(r_value) == INTSXP) {
        result = INTEGER(r_value)[0];
    }

    return result;
}

int is_call_to(const char* function_name, SEXP r_call) {
    SEXP r_function_name = CAR(r_call);
    int library =
        TYPEOF(r_function_name) == SYMSXP &&
        (strcmp(function_name, CHAR(PRINTNAME(r_function_name))) == 0);
    return library;
}
