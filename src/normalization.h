#ifndef EVIL_NORMALIZATION_H
#define EVIL_NORMALIZATION_H
#include <Rincludes.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP r_normalize(SEXP hash, SEXP ast, SEXP trimmed_expr);
SEXP r_normalize_expr(SEXP ast);

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
