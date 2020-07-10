#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP sexp_typeof(SEXP x) {
  return Rf_ScalarInteger(TYPEOF(x));
}
