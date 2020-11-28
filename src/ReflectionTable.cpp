#include "ReflectionTable.h"
#include "r_utilities.h"

static SEXP r_name_ = R_NilValue;

SEXP ReflectionTable::get_name() {
    if(r_name_ == R_NilValue) {
        r_name_ = install("reflection");
    }
    return r_name_;
}

void ReflectionTable::inspect_and_record(ReflectionTable* table,
                                             SEXP r_call,
                                             SEXP r_rho,
                                             int call_id,
                                             int eval_frame_position,
                                             int current_frame_position) {
    if (is_call_to("sys.calls", r_call)) {
        table -> record_call(call_id, "sys.calls");
    }
    else if (is_call_to("sys.frames", r_call)) {
        table -> record_call(call_id, "sys.frames");
    }
    else if (is_call_to("sys.parents", r_call)) {
        table -> record_call(call_id, "sys.parents");
    }
    else if (is_call_to("sys.frame", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, WhichSymbol);
        table -> record_call(call_id, "sys.frame", eval_frame_position, current_frame_position, reverse_frame_index);
    }
    else if (is_call_to("sys.call", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, WhichSymbol);
        table -> record_call(call_id, "sys.call", eval_frame_position, current_frame_position, reverse_frame_index);
    }
    else if (is_call_to("sys.function", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, WhichSymbol);
        table -> record_call(call_id, "sys.function", eval_frame_position, current_frame_position, reverse_frame_index);
    }
    else if (is_call_to("parent.frame", r_call)) {
        int reverse_frame_index = get_argument_as_integer(r_call, r_rho, NSymbol);
        table -> record_call(call_id, "parent.frame", eval_frame_position, current_frame_position, reverse_frame_index);
    }
}


