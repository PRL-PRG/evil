#ifndef EVIL_NORMALIZATION_H
#define EVIL_NORMALIZATION_H
#include <Rincludes.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP r_normalize_expr(SEXP ast);

SEXP r_build_tree(SEXP ast);

SEXP r_tree_to_string(SEXP tree);

SEXP r_simplify(SEXP tree);

typedef enum OpKind {
    UnknownOp,
    ModelFrameOp,
    ArithOp,
    LogicOp,
    NamedOp,
    ListVecOp
} OpKind;

#ifdef __cplusplus
}
#endif

#endif /* EVIL_NORMALIZATION_H */
