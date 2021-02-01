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

const char* from_normalized_type(enum normalized_type ntype) {
    static const char* ntypes[] = {"NUM",
                                   "BOOL",
                                   "STR",
                                   "OP",
                                   "LOGI",
                                   "COMP",
                                   "VAR",
                                   "ENV",
                                   "WREF",
                                   "PTR",
                                   "NULL",
                                   "STROP",
                                   "OTHER"};
    return ntypes[ntype];
}

void write_buffer(char* buffer,
                  int* max_size,
                  int* write_pos,
                  const char* input) {
    for (int i = 0; input[i] != '\0'; i++, (*write_pos)++) {
        if (*write_pos >= *max_size) {
            // Buffer needs to be reallocated!
            // We use the usual exponential strategy
            size_t old_max_size = *max_size;
            *max_size = 2 * (*max_size);
            buffer = (char*) realloc(buffer, *max_size);
            if (buffer != NULL) {
                error("Could not reallocate memory for the normalized "
                      "expression.\n");
            }
            // The content of the new part are undefined so we initialize them
            // to 0
            memset(buffer + old_max_size, '\0', old_max_size);
        }

        buffer[*write_pos] = input[i];
    }
}

void rollback_writepos(char* buffer, int* write_pos, int old_write_pos) {
    memset(buffer + old_write_pos, '\0', *write_pos - old_write_pos);
    *write_pos = old_write_pos;
}

#define NB_ARITH_OP 15
#define NB_STR_OP 3
#define NB_COMP_OP 6
#define NB_BOOL_OP 5

enum normalized_type normalize_expr(SEXP ast,
                                    char* buffer,
                                    int* max_size,
                                    int* write_pos,
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
    case NILSXP:
        write_buffer(buffer, max_size, write_pos, "NULL");
        return N_Null;
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
        Rprintf("Seeing NUM\n");
        write_buffer(buffer, max_size, write_pos, "NUM");
        return N_Num;

    case SYMSXP:
        if (function_call) { // This is a symbol from a function call
            if (in(CHAR(PRINTNAME(ast)), arith_op, NB_ARITH_OP)) {
                write_buffer(buffer, max_size, write_pos, "OP");
                return N_Op;
            } else if (in(CHAR(PRINTNAME(ast)), bool_op, NB_BOOL_OP)) {
                write_buffer(buffer, max_size, write_pos, "LOGI");
                return N_Logi;
            } else if (in(CHAR(PRINTNAME(ast)), cmp_op, NB_COMP_OP)) {
                write_buffer(buffer, max_size, write_pos, "COMP");
                return N_Comp;
              } else if (in(CHAR(PRINTNAME(ast)), str_op, NB_STR_OP)) {
                write_buffer(buffer, max_size, write_pos, CHAR(PRINTNAME(ast)));
                return N_StrOp;
            } else {
                write_buffer(buffer, max_size, write_pos, CHAR(PRINTNAME(ast)));
                return N_Other;
            }
        } else {
            write_buffer(buffer, max_size, write_pos, "VAR");
            return N_Var;
        }

    case LGLSXP:
        write_buffer(buffer, max_size, write_pos, "BOOL");
        return N_Boolean;

    case STRSXP: {
        // Also deal with magic values for env, weak ptr and so on
        const char* s = CHAR(STRING_ELT(ast, 0));

        if (strcmp(s, "<.ENVIRONMENT>") == 0) {
            write_buffer(buffer, max_size, write_pos, "ENV");
            return N_Env;
        } else if (strcmp(s, "<.WEAK REFERENCE>") == 0) {
            write_buffer(buffer, max_size, write_pos, "WREF");
            return N_WRef;
        } else if (strcmp(s, "<.POINTER>") == 0) {
            write_buffer(buffer, max_size, write_pos, "PTR");
            return N_Ptr;
        }
        write_buffer(buffer, max_size, write_pos, "STR");
        return N_String;
    }

    case LANGSXP: {
        SEXP ptr = ast;
        int old_write_pos =
            *write_pos; // Save write pos in case we need to roll back
        // Function name (or anonymous function)
        normalized_type ntype_function =
            normalize_expr(CAR(ptr), buffer, max_size, write_pos, 1);
        normalized_type ntype = N_Other;
        if (ntype_function == N_Comp || ntype_function == N_Op) {
            ntype = N_Num;
        } else if (ntype_function == N_Logi) {
            ntype = N_Boolean;
        }
        else if (ntype_function == N_StrOp) {
            ntype = N_String;
        }
        Rprintf("NType function: %s\n", from_normalized_type(ntype_function));

        write_buffer(buffer, max_size, write_pos, "(");

        // Arguments
        ptr = CDR(ptr);
        while (ptr != R_NilValue) {
            normalized_type ntype_arg =
                normalize_expr(CAR(ptr), buffer, max_size, write_pos, 0);
            Rprintf("NType argument: %s\n", from_normalized_type(ntype_arg));
            if (ntype_arg != ntype) {// Rather write it to crush sequences of similar types.
                ntype = N_Other;
            }
            write_buffer(buffer, max_size, write_pos, ", ");
            ptr = CDR(ptr);
        }

        write_buffer(buffer, max_size, write_pos, ")");

        if (ntype != N_Other) { // Constant folded
            // All the same type so we roll back to previous write position!
            rollback_writepos(buffer, write_pos, old_write_pos);
            if (ntype_function == N_Comp) {
                write_buffer(buffer, max_size, write_pos, "BOOL");
                return N_Boolean;
            } else { // TODO: check if it is a type that makes sense
                write_buffer(
                    buffer, max_size, write_pos, from_normalized_type(ntype));
                return ntype;
            }
        }
        return N_Other;
    }

    case EXPRSXP: {
        int size = Rf_length(ast);
        for (int i = 0; i < size - 1; i++) {
            normalize_expr(VECTOR_ELT(ast, i), buffer, max_size, write_pos, 0);
            write_buffer(buffer, max_size, write_pos, "; ");
        }
        normalize_expr(
            VECTOR_ELT(ast, size - 1), buffer, max_size, write_pos, 0);

        return N_Other;
    }

    case LISTSXP:
        Rprintf("Seeing LIST\n");
        return N_Other;

    default:
        Rprintf("Seeing Other\n");
        return N_Other;
    }

    error("Not supported");
    return N_Other;
}

#define BUF_INIT_SIZE 100
SEXP r_normalize_expr(SEXP ast) {
    // Allocate a buffer  to store the string expression
    // No need to desallocate later because R will take it in charge?
    // No need to initialize to zero after calloc. It's done by the function
    char* buffer = (char*) calloc(BUF_INIT_SIZE, sizeof(char));
    if (buffer == NULL) {
        error("Could not allocate memory for the normalized expression.\n");
    }

    int max_size = BUF_INIT_SIZE;
    int write_pos = 0;

    normalize_expr(ast, buffer, &max_size, &write_pos, 0);
    SEXP r_value = PROTECT(mkString(buffer));
    UNPROTECT(1);
    free(buffer); // mkString copies
    return r_value;
}
