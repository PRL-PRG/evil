#include "Table.h"


SEXP create_data_frame(const std::vector<std::pair<std::string, SEXP>>& columns) {

    int column_count = columns.size();
    int row_count = column_count == 0 ? 0 : Rf_length(columns[0].second);

    SEXP r_data_frame = PROTECT(allocVector(VECSXP, column_count));
    SEXP r_row_names = PROTECT(allocVector(STRSXP, row_count));
    SEXP r_column_names = PROTECT(allocVector(STRSXP, column_count));

        for(int row_index = 0; row_index < row_count; ++row_index) {
            SET_STRING_ELT(r_row_names, row_index, mkChar(std::to_string(row_index + 1).c_str()));
        }

        for(int column_index = 0; column_index < column_count; ++column_index) {
            SET_STRING_ELT(r_column_names, column_index, mkChar(columns[column_index].first.c_str()));
            SET_VECTOR_ELT(r_data_frame, column_index, columns[column_index].second);
        }

        Rf_setAttrib(r_data_frame, R_NamesSymbol, r_column_names);
        Rf_setAttrib(r_data_frame, R_RowNamesSymbol, r_row_names);
        Rf_setAttrib(r_data_frame, R_ClassSymbol, mkString("data.frame"));

        UNPROTECT(3);

        return r_data_frame;
}

