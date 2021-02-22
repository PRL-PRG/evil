#include "r_data.h"
#include "data.h"
#include "ReflectionAnalysis.h"
#include "CodeAnalysis.h"
#include "SideEffectAnalysis.h"
#include "ExecutionTraceAnalysis.h"

template <typename T>
T* unwrap(SEXP r_value) {
    return (T*) (R_ExternalPtrAddr(r_value));
}

template <typename T>
void r_destroy_wrapped_value(SEXP r_value) {
    T* value = unwrap<T>(r_value);
    if (value != NULL) {
        delete value;
        R_SetExternalPtrAddr(r_value, NULL);
    }
}

template <typename T>
SEXP wrap(T* value) {
    SEXP r_value = PROTECT(R_MakeExternalPtr(value, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(r_value, r_destroy_wrapped_value<T>, TRUE);
    UNPROTECT(1);
    return r_value;
}

void initialize_tracer_state(SEXP r_data) {
    SEXP r_state = wrap(new TracerState());
    defineVar(TracerStateSymbol, r_state, r_data);
}

void initialize_tracer_analyses(SEXP r_data) {
    std::vector<SEXP> analyses = {wrap(new ReflectionAnalysis()),
                                  wrap(new CodeAnalysis()),
                                  wrap(new SideEffectAnalysis()),
                                  wrap(new ExecutionTraceAnalysis())};

    int count = analyses.size();

    SEXP r_analysis_list = PROTECT(allocVector(VECSXP, count));

    for (int i = 0; i < count; ++i) {
        SET_VECTOR_ELT(r_analysis_list, i, analyses[i]);
    }

    UNPROTECT(1);

    defineVar(AnalysesSymbol, r_analysis_list, r_data);
}

SEXP r_tracer_data_initialize(SEXP r_data) {
    initialize_tracer_state(r_data);
    initialize_tracer_analyses(r_data);
    return R_NilValue;
}

SEXP r_function_table_initialize(SEXP r_data) {
    TracerState* state = get_tracer_state(r_data);
    state->get_function_table().handle_packages();
    return R_NilValue;
}

TracerState* get_tracer_state(SEXP r_data) {
    SEXP r_state = Rf_findVarInFrame(r_data, TracerStateSymbol);
    return unwrap<TracerState>(r_state);
}

SEXP get_analysis_list(SEXP r_data) {
    return Rf_findVarInFrame(r_data, AnalysesSymbol);
}

std::vector<Analysis*> get_analyses(SEXP r_data) {
    SEXP r_analysis_list = get_analysis_list(r_data);
    int analysis_count = Rf_length(r_analysis_list);
    std::vector<Analysis*> analyses(analysis_count);

    for (int i = 0; i < analysis_count; ++i) {
        analyses[i] = unwrap<Analysis>(VECTOR_ELT(r_analysis_list, i));
    }

    return analyses;
}

SEXP r_tracer_data_finalize(SEXP r_data) {
    SEXP r_analysis_list = get_analysis_list(r_data);
    std::vector<std::vector<Table*>> tables;
    int table_count = 0;

    int analysis_count = Rf_length(r_analysis_list);

    for (int i = 0; i < analysis_count; ++i) {
        SEXP r_analysis = VECTOR_ELT(r_analysis_list, i);
        Analysis* analysis = unwrap<Analysis>(r_analysis);
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

SEXP r_tracer_data_eval_call_entry(SEXP r_data,
                                   SEXP r_call_id,
                                   SEXP r_env,
                                   SEXP r_frame_depth) {
    TracerState* state = get_tracer_state(r_data);
    int call_id = asInteger(r_call_id);
    int frame_depth = asInteger(r_frame_depth);

    state->set_eval_call_info(call_id, r_env, frame_depth);

    return R_NilValue;
}

SEXP r_tracer_data_eval_call_exit(SEXP r_data) {
    TracerState* state = get_tracer_state(r_data);
    int interp_eval = state->pop_interp_eval_count();
    return ScalarInteger(interp_eval);
}

SEXP r_tracer_data_add_package(SEXP r_data, SEXP r_package_name) {
    TracerState* state = get_tracer_state(r_data);
    state->get_function_table().handle_packages();
    return R_NilValue;
}
