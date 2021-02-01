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

int in(const char* target, const char** array, int array_length) {
    for (int i = 0; i < array_length; i++) {
        if (strcmp(array[i], target) == 0) {
            return 1;
        }
    }
    return 0;
}

enum normalized_type to_normalized_type(const char* val) {
    if (strcmp(val, "NUM")) {
        return N_Num;
    } else if (strcmp(val, "BOOL")) {
        return N_Boolean;
    } else if (strcmp(val, "STR")) {
        return N_String;
    } else {
        return N_Other;
    }
}

const char* from_normalized_type(enum normalized_type ntype) {
    static const char* ntypes[] = {"NUM", "BOOL", "STR", "OTHER"};
    return ntypes[ntype];
}

#define NB_ARITH_OP 15
#define NB_STR_OP 3
#define NB_COMP_OP 6
#define NB_BOOL_OP 5

enum normalized_type normalize_expr(SEXP ast,
                                    char* buffer,
                                    int *max_size,
                                    int *write_pos,
                                    int function_call) {
    static const char* arith_op[NB_ARITH_OP] = {"/",
                                                "-",
                                                "*",
                                                "+",
                                                "^",
                                                "log",
                                                "sqrt",
                                                "exp",
                                                "max",
                                                "min",
                                                "cos",
                                                "sin",
                                                "abs",
                                                "atan",
                                                ":"};

    static const char* str_op[NB_STR_OP] = {"paste", "paste0", "str_c"};

    static const char* cmp_op[NB_COMP_OP] = {"<", ">", "<=", ">=", "==", "!="};

    static const char* bool_op[NB_BOOL_OP] = {"&", "&&", "|", "||", "!"};

    switch (TYPEOF(ast)) {
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
        return N_Num;

    case SYMSXP:
        if (function_call) { // This is a symbol from a function call
            if (in(CHAR(PRINTNAME(ast)), arith_op, NB_ARITH_OP)) {
                return "OP";
            } else if (in(CHAR(PRINTNAME(ast)), bool_op, NB_BOOL_OP)) {
                return "LOGI";
            } else if (in(CHAR(PRINTNAME(ast)), cmp_op, NB_COMP_OP)) {
                return "COMP";
            } else {
                return (CHAR(PRINTNAME(ast)));
            }
        } else {
            return "VAR";
        }

    case LGLSXP:
        return "BOOL";

    case STRSXP:
        // Also deal with magic values for emv, weak ptr and so on
        return "STR";

    case LANGSXP: {
        SEXP ptr = ast;
        // Function name (or anonymous function)
        const char* function_name = normalize_expr(CAR(ptr), 1);
        enum normalized_type ntype = N_Other;
        if (strcmp(function_name, "OP") == 0) {
            ntype = N_Num;
        } else if (in(function_name, str_op, NB_STR_OP)) {
            ntype = N_String;
        } else if (strcmp(function_name, "LOGI") == 0) {
            ntype = N_Boolean;
        } else if (strcmp(function_name, "COMP") == 0) {
            ntype = N_Num;
        }
        // Arguments
        ptr = CDR(ptr);
        while (ptr != R_NilValue) {
            normalized_type ntype_arg = normalize_expr(ptr, 0);
            if (ntype_arg != ntype) {
                ntype = N_Other;
            }
            ptr = CDR(ptr);
        }
        if (ntype != N_Other) {
            if (function_name == "COMP") {
                return "BOOL";
            } else {
                return from_normalized_type(ntype);
            }
        }
        return "PLOP";
    }

    case EXPRSXP: {
        int size = Rf_length(ast);
        for (int i = 0; i < size - 1; i++) {
            normalize_expr(VECTOR_ELT(ast, i), buffer, max_size, write_pos, 0);
            write_buffer(buffer)
        }
        normalize_expr(VECTOR_ELT(ast, i), 0);

        return N_Other;
    }

    case LISTSXP:
        return "";

    default:
        return "";
    }

    return "";
}

#define BUF_INIT_SIZE 100
SEXP r_normalize_expr(SEXP ast) {
    // Allocate a buffer  to store the string expression
    // No need to desallocate later because R will take it in charge?
    // No need to initialize to zero after calloc. It's done by the function
    char* buffer = (char*) calloc(BUF_INIT_SIZE, sizeof(char));

    normalize_expr(ast, buffer, BUF_INIT_SIZE, 0, 0);
    SEXP r_value = PROTECT(mkString(buffer));
    UNPROTECT(1);
    free(buffer); // mkString copies
    return r_value;
}
