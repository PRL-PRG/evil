#include "Table.h"

SEXP create_data_frame(
    const std::vector<std::pair<std::string, SEXP>>& columns) {
    int column_count = columns.size();
    int row_count = column_count == 0 ? 0 : Rf_length(columns[0].second);

    SEXP r_data_frame = PROTECT(allocVector(VECSXP, column_count));
    SEXP r_row_names = PROTECT(allocVector(STRSXP, row_count));
    SEXP r_column_names = PROTECT(allocVector(STRSXP, column_count));

    for (int row_index = 0; row_index < row_count; ++row_index) {
        SET_STRING_ELT(r_row_names,
                       row_index,
                       mkChar(std::to_string(row_index + 1).c_str()));
    }

    for (int column_index = 0; column_index < column_count; ++column_index) {
        SET_STRING_ELT(r_column_names,
                       column_index,
                       mkChar(columns[column_index].first.c_str()));
        SET_VECTOR_ELT(
            r_data_frame, column_index, columns[column_index].second);
    }

    Rf_setAttrib(r_data_frame, R_NamesSymbol, r_column_names);
    Rf_setAttrib(r_data_frame, R_RowNamesSymbol, r_row_names);
    Rf_setAttrib(r_data_frame, R_ClassSymbol, mkString("data.frame"));

    UNPROTECT(3);

    return r_data_frame;
}

SEXP create_logical_vector(const std::vector<int>& elements) {
    int element_count = elements.size();
    SEXP r_elements = PROTECT(allocVector(LGLSXP, element_count));

    for (int index = 0; index < element_count; ++index) {
        LOGICAL(r_elements)[index] = elements[index];
    }

    UNPROTECT(1);

    return r_elements;
}

SEXP create_integer_vector(const std::vector<int>& elements) {
    int element_count = elements.size();
    SEXP r_elements = PROTECT(allocVector(INTSXP, element_count));

    for (int index = 0; index < element_count; ++index) {
        INTEGER(r_elements)[index] = elements[index];
    }

    UNPROTECT(1);

    return r_elements;
}

SEXP create_double_vector(const std::vector<double>& elements) {
    int element_count = elements.size();
    SEXP r_elements = PROTECT(allocVector(REALSXP, element_count));

    for (int index = 0; index < element_count; ++index) {
        REAL(r_elements)[index] = elements[index];
    }

    UNPROTECT(1);

    return r_elements;
}

SEXP create_character_vector(const std::vector<std::string>& elements) {
    int element_count = elements.size();
    SEXP r_elements = PROTECT(allocVector(STRSXP, element_count));

    for (int index = 0; index < element_count; ++index) {
        SEXP r_element = elements[index] == MissingStringValue
                             ? NA_STRING
                             : mkChar(elements[index].c_str());
        SET_STRING_ELT(r_elements, index, r_element);
    }

    UNPROTECT(1);

    return r_elements;
}
