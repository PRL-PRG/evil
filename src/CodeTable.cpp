#include "CodeTable.h"
#include "r_utilities.h"

static SEXP r_name_ = R_NilValue;

SEXP CodeTable::get_name() {
    if(r_name_ == R_NilValue) {
        r_name_ = install("code");
    }
    return r_name_;
}
