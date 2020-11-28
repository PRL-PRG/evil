#ifndef EVIL_REFLECTION_TABLE_H
#define EVIL_REFLECTION_TABLE_H

#include <vector>
#include <string>
#include "r_init.h"

class ReflectionTable {
public:
    ReflectionTable() {
    }

    void record_call(int call_id, const char* function_name,
                      int eval_frame_index = NA_INTEGER,
                      int call_frame_index = NA_INTEGER,
                      int reverse_frame_index = NA_INTEGER) {
        call_ids_.push_back(call_id);
        function_names_.push_back(function_name);
        eval_frame_depths_.push_back(eval_frame_index);
        call_frame_depths_.push_back(call_frame_index);
        int accessed_frame_index = reverse_frame_index == NA_INTEGER ? NA_INTEGER : call_frame_index - accessed_frame_index;
        accessed_frame_depths_.push_back(accessed_frame_index);
        int leaks = NA_LOGICAL;
        if(eval_frame_index != NA_INTEGER && call_frame_index != NA_INTEGER && accessed_frame_index != NA_INTEGER) {
            leaks = accessed_frame_index <= eval_frame_index;
        }
        leaky_.push_back(leaks);
    }

    SEXP to_data_frame() {

        int column_size = 6;
        int row_size = call_ids_.size();

        SEXP r_data_frame = PROTECT(allocVector(VECSXP, column_size));
        SEXP r_row_names = PROTECT(allocVector(STRSXP, row_size));
        SEXP r_column_names = PROTECT(allocVector(STRSXP, column_size));

        SEXP r_call_ids = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_function_names = PROTECT(allocVector(STRSXP, row_size));
        SEXP r_eval_frame_depth = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_call_frame_depth = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_accessed_frame_depth = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_leaky = PROTECT(allocVector(INTSXP, row_size));

        for(int row_index = 0; row_index < row_size; ++row_index) {
            INTEGER(r_call_ids)[row_index] = call_ids_[row_index];
            SET_STRING_ELT(r_function_names, row_index, mkChar(function_names_[row_index].c_str()));
            INTEGER(r_eval_frame_depth)[row_index] = eval_frame_depths_[row_index];
            INTEGER(r_call_frame_depth)[row_index] = call_frame_depths_[row_index];
            INTEGER(r_accessed_frame_depth)[row_index] = accessed_frame_depths_[row_index];
            LOGICAL(r_leaky)[row_index] = leaky_[row_index];
            SET_STRING_ELT(r_row_names, row_index, mkChar(std::to_string(row_index + 1).c_str()));
        }

        SET_STRING_ELT(r_column_names, 0, mkChar("call_id"));
        SET_STRING_ELT(r_column_names, 1, mkChar("function_name"));
        SET_STRING_ELT(r_column_names, 2, mkChar("eval_frame_depth"));
        SET_STRING_ELT(r_column_names, 3, mkChar("call_frame_depth"));
        SET_STRING_ELT(r_column_names, 4, mkChar("accessed_frame_depth"));
        SET_STRING_ELT(r_column_names, 5, mkChar("leaky"));

        SET_VECTOR_ELT(r_data_frame, 0, r_call_ids);
        SET_VECTOR_ELT(r_data_frame, 1, r_function_names);
        SET_VECTOR_ELT(r_data_frame, 2, r_eval_frame_depth);
        SET_VECTOR_ELT(r_data_frame, 3, r_call_frame_depth);
        SET_VECTOR_ELT(r_data_frame, 4, r_accessed_frame_depth);
        SET_VECTOR_ELT(r_data_frame, 5, r_leaky);

        Rf_setAttrib(r_data_frame, R_NamesSymbol, r_column_names);
        Rf_setAttrib(r_data_frame, R_RowNamesSymbol, r_row_names);
        Rf_setAttrib(r_data_frame, R_ClassSymbol, mkString("data.frame"));

        UNPROTECT(9);

        return r_data_frame;
    }

private:
    std::vector<int> call_ids_;
    std::vector<std::string> function_names_;
    std::vector<int> eval_frame_depths_;
    std::vector<int> call_frame_depths_;
    std::vector<int> accessed_frame_depths_;
    std::vector<int> leaky_;
};

#endif /* EVIL_REFLECTION_TABLE_H */
