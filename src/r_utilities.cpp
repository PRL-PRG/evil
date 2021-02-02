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

const char* from_normalized_type(normalized_type ntype) {
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
                                   "LISTVEC",
                                   "MODEL.FRAME",
                                   "::",
                                   "OTHER"};
    return ntypes[ntype];
}

void write_buffer(char** buffer,
                  int* max_size,
                  int* write_pos,
                  const char* input) {
    for (int i = 0; input[i] != '\0'; i++, (*write_pos)++) {
        if (*write_pos >= *max_size) {
            // Buffer needs to be reallocated!
            // We use the usual exponential strategy
            size_t old_max_size = *max_size;
            *max_size = 2 * (*max_size);
            char* old_buffer = *buffer;
            *buffer = (char*) realloc(*buffer, *max_size * sizeof(char));
            if (*buffer == NULL) {
                free(old_buffer);
                error("Could not reallocate memory for the normalized "
                      "expression.\n");
            }
            // The content of the new part are undefined so we initialize them
            // to 0
            memset(*buffer + old_max_size, '\0', old_max_size);
        }

        (*buffer)[*write_pos] = input[i];
    }
}

void rollback_writepos(char** buffer, int* write_pos, int old_write_pos) {
    memset(*buffer + old_write_pos, '\0', *write_pos - old_write_pos);
    *write_pos = old_write_pos;
}

#define NB_ARITH_OP 15
#define NB_STR_OP 3
#define NB_COMP_OP 6
#define NB_BOOL_OP 5
#define NB_LISTVEC 2

// TODO: merge previous_ntype and function_call?
normalized_type normalize_expr(SEXP ast,
                               char** buffer,
                               int* max_size,
                               int* write_pos,
                               int function_call,
                               normalized_type previous_ntype) {
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

    static const char* listvec[NB_LISTVEC] = {"list", "c"};

    switch (TYPEOF(ast)) {
    case NILSXP: {
        if (previous_ntype != N_Num) {
            write_buffer(buffer, max_size, write_pos, "NULL");
        }

        return N_Null;
    }

    case INTSXP:
    case REALSXP:
    case CPLXSXP: {
        if (previous_ntype != N_Num) {
            write_buffer(buffer, max_size, write_pos, "NUM");
        }
        return N_Num;
    }

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
            } else if (in(CHAR(PRINTNAME(ast)), listvec, NB_LISTVEC)) {
                write_buffer(buffer, max_size, write_pos, CHAR(PRINTNAME(ast)));
                return N_ListVec;
            } else if (strcmp(CHAR(PRINTNAME(ast)), "model.frame") == 0) {
                write_buffer(buffer, max_size, write_pos, "model.frame");
                return N_ModelFrame;
            } 
            else if (strcmp(CHAR(PRINTNAME(ast)), "::") == 0) {
                // We want to get rid of the namespaces
                return N_Namespace;
            }
            else {
                write_buffer(buffer, max_size, write_pos, CHAR(PRINTNAME(ast)));
                return N_Other;
            }
        } else {
            if (previous_ntype != N_Var) {
                write_buffer(buffer, max_size, write_pos, "VAR");
            }
            return N_Var;
        }

    case LGLSXP: {
        if (previous_ntype != N_Boolean) {
            write_buffer(buffer, max_size, write_pos, "BOOL");
        }
        return N_Boolean;
    }

    case STRSXP: {
        // This comes from the parser and so there is always one CHARSXP in the
        // character vector
        const char* s = CHAR(STRING_ELT(ast, 0));

        if (strcmp(s, "<.ENVIRONMENT>") == 0) {
            if (previous_ntype != N_Env) {
                write_buffer(buffer, max_size, write_pos, "ENV");
            }
            return N_Env;
        } else if (strcmp(s, "<.WEAK REFERENCE>") == 0) {
            if (previous_ntype != N_WRef) {
                write_buffer(buffer, max_size, write_pos, "WREF");
            }
            return N_WRef;
        } else if (strcmp(s, "<.POINTER>") == 0) {
            if (previous_ntype != N_Ptr) {
                write_buffer(buffer, max_size, write_pos, "PTR");
            }
            return N_Ptr;
        }

        if (previous_ntype != N_String) {
            write_buffer(buffer, max_size, write_pos, "STR");
        }
        return N_String;
    }

    case LISTSXP:
    case LANGSXP: {
        SEXP ptr = ast;
        int old_write_pos =
            *write_pos; // Save write pos in case we need to roll back
        // Function name (or anonymous function)
        normalized_type ntype_function =
            normalize_expr(CAR(ptr), buffer, max_size, write_pos, 1, N_Other);

        if(ntype_function == N_Namespace) {
            // We skip the namespace name!
            ptr = CDDR(ptr);
            return normalize_expr(CAR(ptr), buffer, max_size, write_pos, 1, N_Other);
        }
        
        normalized_type ntype = N_Other;
        if (ntype_function == N_Comp || ntype_function == N_Op) {
            ntype = N_Num;
        } else if (ntype_function == N_Logi) {
            ntype = N_Boolean;
        } else if (ntype_function == N_StrOp) {
            ntype = N_String;
        }
        // Rprintf("NType function: %s\n",
        // from_normalized_type(ntype_function));

        write_buffer(buffer, max_size, write_pos, "(");

        // Arguments
        ptr = CDR(ptr);
        int i = 0;
        int isVar = 0; // is there at least one Var?
        int old_write_pos2 = *write_pos;
        normalized_type ntype_arg = N_Other;
        while (ptr != R_NilValue) {
            int old_write_pos3 = *write_pos;
            ntype_arg = normalize_expr(CAR(ptr),
                                       buffer,
                                       max_size,
                                       write_pos,
                                       0,
                                       (ntype_function == N_ListVec) ? ntype_arg
                                                                     : N_Other);
            // Rprintf("NType argument: %s\n", from_normalized_type(ntype_arg));

            // To merge all similar elements in a list or vector, or do VAR
            // absorption
            if (i == 0 && ntype_function == N_ListVec) {
                ntype = ntype_arg;
            }

            if (ntype_arg != ntype && ntype_arg != N_Var) {
                // Rather write it to crush sequences of similar types.
                ntype = N_Other;
            }
            if (ntype_arg == N_Var) {
                isVar = 1;
            }
            ptr = CDR(ptr);
            i++;
            // A bit ugly..., to not write that comma at the end
            // Another solution would be to rollback the buffer by 2 characters
            // after the loop But is it efficient? And it also feels hacky...
            // And we put a comma only if we wrote something after the previous
            // comma...
            if (ptr != R_NilValue && old_write_pos3 != *write_pos) {
                write_buffer(buffer, max_size, write_pos, ", ");
            }
        }

        if (ntype != N_Other) { // Constant folded and VAR absorption
            if (isVar) {
                rollback_writepos(buffer, write_pos, old_write_pos2);
                write_buffer(buffer, max_size, write_pos, "VAR");
            } else if (ntype_function == N_ListVec) {
                rollback_writepos(buffer, write_pos, old_write_pos2);
                write_buffer(
                    buffer, max_size, write_pos, from_normalized_type(ntype));
            } else {
                // All the same type so we roll back to previous write position!
                rollback_writepos(buffer, write_pos, old_write_pos);
                if (ntype_function == N_Comp) {
                    write_buffer(buffer, max_size, write_pos, "BOOL");
                    return N_Boolean;
                } else { // TODO: check if it is a type that makes sense
                    write_buffer(buffer,
                                 max_size,
                                 write_pos,
                                 from_normalized_type(ntype));
                    return ntype;
                }
            }
        }

        write_buffer(buffer, max_size, write_pos, ")");
        return N_Other;
    }

    case EXPRSXP: {
        int size = Rf_length(ast);
        for (int i = 0; i < size - 1; i++) {
            normalize_expr(
                VECTOR_ELT(ast, i), buffer, max_size, write_pos, 0, N_Other);
            write_buffer(buffer, max_size, write_pos, "; ");
        }
        normalize_expr(
            VECTOR_ELT(ast, size - 1), buffer, max_size, write_pos, 0, N_Other);

        return N_Other;
    }

    case DOTSXP: {
        write_buffer(buffer, max_size, write_pos, "...");
        return N_Other;
    }

    // The following cases should not happen ars they are not deparsable
    // The yare handled by magic values in STRSXP
    case BCODESXP: {
        write_buffer(buffer, max_size, write_pos, "BYTECODE");
        return N_Other;
    }

    case RAWSXP: {
        write_buffer(buffer, max_size, write_pos, "RAWVEC");
        return N_Other;
    }

    case EXTPTRSXP: {
        write_buffer(buffer, max_size, write_pos, "PRT");
        return N_Other;
    }

    case WEAKREFSXP: {
        write_buffer(buffer, max_size, write_pos, "WREF");
        return N_Other;
    }

    case ENVSXP: {
        write_buffer(buffer, max_size, write_pos, "ENV");
        return N_Other;
    }

    default:
        Rprintf("Seeing Unexpected SEXP.\n");
        return N_Other;
    }

    error("Not supported");
    return N_Other;
}

#define BUF_INIT_SIZE 100
SEXP r_normalize_expr(SEXP ast) {
    // Allocate a buffer  to store the string expression
    // No need to initialize to zero after calloc. It's done by the function
    char* buffer = (char*) calloc(BUF_INIT_SIZE, sizeof(char));
    if (buffer == NULL) {
        error("Could not allocate memory for the normalized expression.\n");
    }

    int max_size = BUF_INIT_SIZE;
    int write_pos = 0;

    // The address pointing to buffer will be changed by realloc!
    // So the initial buffer will be invalidated and we need to keep track of it
    normalize_expr(ast, &buffer, &max_size, &write_pos, 0, N_Other);
    SEXP r_value = PROTECT(mkString(buffer));
    UNPROTECT(1);
    free(buffer); // mkString copies
    return r_value;
}
