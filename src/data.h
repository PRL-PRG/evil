#ifndef EVIL_DATA_H
#define EVIL_DATA_H

#include <R.h>
#include "ReflectionTable.h"
#include "CodeTable.h"

template<typename T>
T* get_table(SEXP r_data) {
    SEXP r_table = Rf_findVarInFrame(r_data, T::get_name());
    T* table = (T*)(R_ExternalPtrAddr(r_table));
    return table;
}

#endif /* EVIL_DATA_H */