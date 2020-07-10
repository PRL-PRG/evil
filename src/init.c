#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL

extern SEXP sexp_typeof(SEXP);

static const R_CallMethodDef callMethods[] = {
    {"sexp_typeof", (DL_FUNC) &sexp_typeof, 1},
    {NULL, NULL, 0}};

void R_init_evil(DllInfo* dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);

    R_useDynamicSymbols(dll, FALSE);
}
