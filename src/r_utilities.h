#ifndef EVIL_R_UTILITIES_H
#define EVIL_R_UTILITIES_H

#include <Rincludes.h>

extern const char* MissingStringValue;

#ifdef __cplusplus
extern "C" {
#endif

SEXP sexp_typeof(SEXP x);
SEXP mark_parsed_expression(SEXP x, SEXP parse_fun_name);
SEXP r_get_ast_size(SEXP ast);
int get_sexp_type(SEXP r_value, int follow_symbol);
SEXP r_normalize_expr(SEXP ast);

enum normalized_type {
    N_Num,
    N_Boolean,
    N_String,
    N_Op,
    N_Logi,
    N_Comp,
    N_Var,
    N_Env,
    N_WRef,
    N_Ptr,
    N_Null,
    N_StrOp,
    N_ListVec,
    N_Other
};
typedef enum normalized_type normalized_type;

#ifdef __cplusplus
}
#endif

#endif /* EVIL_R_UTILITIES_H */
