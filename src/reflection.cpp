#include "reflection.h"
#include "r_reflection.h"
#include "r_utilities.h"
#undef length
#include <iostream>

void inspect_for_reflective_call(ReflectionTable* reflection_table,
                                 SEXP r_call,
                                 SEXP r_rho,
                                 int call_id,
                                 int eval_frame_position,
                                 int current_frame_position) {
    if (is_call_to("sys.calls", r_call)) {
        reflection_table -> record_call(call_id, "sys.calls");
    }
    else if (is_call_to("sys.frames", r_call)) {
        reflection_table -> record_call(call_id, "sys.frames");
    }
    else if (is_call_to("sys.parents", r_call)) {
        reflection_table -> record_call(call_id, "sys.parents");
    }
    else if (is_call_to("sys.frame", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, WhichSymbol);
        reflection_table -> record_call(call_id, "sys.frame", eval_frame_position, current_frame_position, reverse_frame_index);
    }
    else if (is_call_to("sys.call", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, WhichSymbol);
        reflection_table -> record_call(call_id, "sys.call", eval_frame_position, current_frame_position, reverse_frame_index);
    }
    else if (is_call_to("sys.function", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, WhichSymbol);
        reflection_table -> record_call(call_id, "sys.function", eval_frame_position, current_frame_position, reverse_frame_index);
    }
    else if (is_call_to("parent.frame", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, NSymbol);
        reflection_table -> record_call(call_id, "parent.frame", eval_frame_position, current_frame_position, reverse_frame_index);
    }
}

void r_destroy_reflection_table(SEXP r_reflection_table) {
    void *ptr = R_ExternalPtrAddr(r_reflection_table);
    if(ptr != NULL) {
        ReflectionTable* table = (ReflectionTable*)(ptr);
        delete table;
        R_SetExternalPtrAddr(r_reflection_table, NULL);
    }
}

SEXP r_create_reflection_table() {
    ReflectionTable* table = new ReflectionTable();
    SEXP r_table = R_MakeExternalPtr(table, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(r_table, r_destroy_reflection_table, TRUE);
    return r_table;
}

SEXP r_reflection_table_to_data_frame(SEXP r_reflection_table) {
    void *ptr = R_ExternalPtrAddr(r_reflection_table);
    if(ptr != NULL) {
        ReflectionTable* table = (ReflectionTable*)(ptr);
        return table -> to_data_frame();
    }
    return R_NilValue;
}
