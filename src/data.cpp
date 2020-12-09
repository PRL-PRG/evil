#include "r_data.h"
#include "data.h"
#include "ReflectionAnalysis.h"
#include "CodeAnalysis.h"
#include "SideEffectAnalysis.h"

Analysis* unwrap_analysis(SEXP r_analysis) {
    return (Analysis*) (R_ExternalPtrAddr(r_analysis));
}

void r_destroy_analysis(SEXP r_analysis) {
    Analysis* analysis = unwrap_analysis(r_analysis);
    if (analysis != NULL) {
        delete analysis;
        R_SetExternalPtrAddr(r_analysis, NULL);
    }
}

SEXP wrap_analysis(Analysis* analysis) {
    SEXP r_analysis =
        PROTECT(R_MakeExternalPtr(analysis, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(r_analysis, r_destroy_analysis, TRUE);
    UNPROTECT(1);
    return r_analysis;
}

template <typename T>
SEXP create_analysis() {
    T* analysis = new T();
    return wrap_analysis(analysis);
}

SEXP r_initialize_analyses(SEXP r_data) {
    std::vector<SEXP> analyses = {create_analysis<ReflectionAnalysis>(),
                                  create_analysis<CodeAnalysis>(),
                                  create_analysis<SideEffectAnalysis>()};

    int count = analyses.size();

    SEXP r_analysis_list = PROTECT(allocVector(VECSXP, count));

    for (int i = 0; i < count; ++i) {
        SET_VECTOR_ELT(r_analysis_list, i, analyses[i]);
    }

    UNPROTECT(1);

    defineVar(AnalysesSymbol, r_analysis_list, r_data);

    return R_NilValue;
}

SEXP get_analysis_list(SEXP r_data) {
    return Rf_findVarInFrame(r_data, AnalysesSymbol);
}

std::vector<Analysis*> get_analyses(SEXP r_data) {
    SEXP r_analysis_list = get_analysis_list(r_data);
    int analysis_count = Rf_length(r_analysis_list);
    std::vector<Analysis*> analyses(analysis_count);

    for (int i = 0; i < analysis_count; ++i) {
        analyses[i] = unwrap_analysis(VECTOR_ELT(r_analysis_list, i));
    }

    return analyses;
}

SEXP r_get_tables(SEXP r_data) {
    SEXP r_analysis_list = get_analysis_list(r_data);
    std::vector<std::vector<Table*>> tables;
    int table_count = 0;

    int analysis_count = Rf_length(r_analysis_list);

    for (int i = 0; i < analysis_count; ++i) {
        SEXP r_analysis = VECTOR_ELT(r_analysis_list, i);
        Analysis* analysis = unwrap_analysis(r_analysis);
        auto analysis_tables = analysis->get_tables();
        tables.push_back(analysis_tables);
        table_count += analysis_tables.size();
    }

    SEXP r_table_list = PROTECT(allocVector(VECSXP, table_count));
    SEXP r_table_names = PROTECT(allocVector(STRSXP, table_count));

    int table_index = 0;
    for (int analysis_index = 0; analysis_index < tables.size();
         ++analysis_index) {
        std::vector<Table*> analysis_tables = tables[analysis_index];
        for (int i = 0; i < analysis_tables.size(); ++i) {
            Table* table = analysis_tables[i];
            SET_VECTOR_ELT(r_table_list, table_index, table->as_data_frame());
            SET_STRING_ELT(
                r_table_names, table_index, mkChar(table->get_name().c_str()));
            ++table_index;
        }
    }

    Rf_setAttrib(r_table_list, R_NamesSymbol, r_table_names);

    UNPROTECT(2);

    return r_table_list;
}
