#ifndef EVIL_CODE_TABLE_H
#define EVIL_CODE_TABLE_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Table.h"

class CodeTable: public Table {
public:
    CodeTable(): Table() {
    }

    void record_call(int call_id,
                     const char* function,
                     const char* description) {
        call_ids_.push_back(call_id);
        functions_.push_back(function);
        descriptions_.push_back(description);
    }

    SEXP to_data_frame() override {

        int row_size = call_ids_.size();

        SEXP r_call_ids = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_functions = PROTECT(allocVector(STRSXP, row_size));
        SEXP r_descriptions = PROTECT(allocVector(STRSXP, row_size));

        for(int row_index = 0; row_index < row_size; ++row_index) {
            INTEGER(r_call_ids)[row_index] = call_ids_[row_index];
            SET_STRING_ELT(r_functions, row_index, mkChar(functions_[row_index].c_str()));
            SET_STRING_ELT(r_descriptions, row_index, mkChar(descriptions_[row_index].c_str()));
        }

        SEXP r_data_frame = create_data_frame({{"call_id", r_call_ids},
                                               {"function", r_functions},
                                               {"description", r_descriptions}});

        UNPROTECT(3);

        return r_data_frame;
    }

    static void inspect_and_record(CodeTable* table,
                                 SEXP r_call,
                                 SEXP r_rho,
                                 int call_id);
    static SEXP get_name();

private:
    std::vector<int> call_ids_;
    std::vector<std::string> functions_;
    std::vector<std::string> descriptions_;
};

#endif /* EVIL_CODE_TABLE_H */
