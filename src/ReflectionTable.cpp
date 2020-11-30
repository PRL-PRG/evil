#include "ReflectionTable.h"
#include "r_utilities.h"

static SEXP r_name_ = R_NilValue;

SEXP ReflectionTable::get_name() {
    if(r_name_ == R_NilValue) {
        r_name_ = install("reflection");
    }
    return r_name_;
}


