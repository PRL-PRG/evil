#include "r_data.h"
#include "data.h"

void r_destroy_table(SEXP r_table) {
    void *ptr = R_ExternalPtrAddr(r_table);
    if(ptr != NULL) {
        Table* table = (Table*)(ptr);
        delete table;
        R_SetExternalPtrAddr(r_table, NULL);
    }
}

template <typename T>
SEXP initialize_table(SEXP r_data) {
    T* table = new T();
    SEXP r_table = PROTECT(R_MakeExternalPtr(table, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(r_table, r_destroy_table, TRUE);
    defineVar(T::get_name(), r_table, r_data);
    UNPROTECT(1);
    return r_table;
}

SEXP r_initialize_tables(SEXP r_data) {
    initialize_table<ReflectionTable>(r_data);
    initialize_table<CodeTable>(r_data);
}

SEXP r_get_tables(SEXP r_data) {
    std::vector<std::pair<SEXP, Table*>> tables = {
                                                  {ReflectionTable::get_name(), get_table<ReflectionTable>(r_data)},
                                                  {CodeTable::get_name(), get_table<CodeTable>(r_data)}
    };

    int count = tables.size();

    SEXP r_result = PROTECT(allocVector(VECSXP, count));
    SEXP r_names = PROTECT(allocVector(STRSXP, count));

    for(int i = 0; i < count; ++i) {
        SET_VECTOR_ELT(r_result, i, tables[i].second->to_data_frame());
        SET_STRING_ELT(r_names, i, mkChar(CHAR(PRINTNAME(tables[i].first))));
    }

    Rf_setAttrib(r_result, R_NamesSymbol, r_names);

    UNPROTECT(2);

    return r_result;
}
