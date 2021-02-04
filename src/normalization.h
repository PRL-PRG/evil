#ifndef EVIL_NORMALIZATION_H
#define EVIL_NORMALIZATION_H

#include <Rincludes.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP r_normalize_expr(SEXP ast);

enum NTYPE {
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
    N_ModelFrame,
    N_Namespace,
    N_Function,
    N_Paren,
    N_NA,
    N_Other
};

  typedef enum NTYPE NTYPE;

#ifdef __cplusplus
}
#endif

#endif /* EVIL_NORMALIZATION_H */
