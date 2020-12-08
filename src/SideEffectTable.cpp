#include "SideEffectTable.h"
#include "r_utilities.h"

static SEXP r_name_ = R_NilValue;

SEXP SideEffectTable::get_name() {
    if (r_name_ == R_NilValue) {
        r_name_ = install("side_effect");
    }
    return r_name_;
}
