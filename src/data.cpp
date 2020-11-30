#include "r_data.h"
#include "data.h"

Table* unwrap_table(SEXP r_table) {
    return (Table*) (R_ExternalPtrAddr(r_table));
}

void r_destroy_table(SEXP r_table) {
    Table *table = unwrap_table(r_table);
    if(table != NULL) {
        delete table;
        R_SetExternalPtrAddr(r_table, NULL);
    }
}

SEXP wrap_table(Table* table) {
    SEXP r_table = PROTECT(R_MakeExternalPtr(table, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(r_table, r_destroy_table, TRUE);
    UNPROTECT(1);
    return r_table;
}

template <typename T>
SEXP create_table() {
    T* table = new T();
    return wrap_table(table);
}

SEXP r_initialize_tables(SEXP r_data) {
    std::vector<std::pair<SEXP, SEXP>> tables = {
        {ReflectionTable::get_name(), create_table<ReflectionTable>()},
        {CodeTable::get_name(), create_table<CodeTable>()}};

    int count = tables.size();

    SEXP r_table_names = PROTECT(allocVector(STRSXP, count));
    SEXP r_table_list = PROTECT(allocVector(VECSXP, count));

    for (int i = 0; i < count; ++i) {
        SET_STRING_ELT(
            r_table_names, i, mkChar(CHAR(PRINTNAME(tables[i].first))));
        SET_VECTOR_ELT(r_table_list, i, tables[i].second);
    }

    Rf_setAttrib(r_table_list, R_NamesSymbol, r_table_names);

    UNPROTECT(2);

    defineVar(TablesSymbol, r_table_list, r_data);

    return R_NilValue;
}

SEXP get_table_list(SEXP r_data) {
    return Rf_findVarInFrame(r_data, TablesSymbol);
}

std::vector<Table*> get_tables(SEXP r_data) {
    SEXP r_table_list = get_table_list(r_data);
    int table_count = Rf_length(r_table_list);
    std::vector<Table*> tables(table_count);

    for (int i = 0; i < table_count; ++i) {
        tables[i] = unwrap_table(VECTOR_ELT(r_table_list, i));
    }

    return tables;
}

SEXP r_get_tables_as_data_frames(SEXP r_data) {
    SEXP r_table_list = get_table_list(r_data);
    SEXP r_table_names = Rf_getAttrib(r_table_list, R_NamesSymbol);

    int count = Rf_length(r_table_list);

    SEXP r_result = PROTECT(allocVector(VECSXP, count));

    for(int i = 0; i < count; ++i) {
        SEXP r_table = VECTOR_ELT(r_table_list, i);
        Table* table = unwrap_table(r_table);
        SET_VECTOR_ELT(r_result, i, table -> to_data_frame());
    }

    Rf_setAttrib(r_result, R_NamesSymbol, r_table_names);

    UNPROTECT(1);

    return r_result;
}
