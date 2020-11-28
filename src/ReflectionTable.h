#ifndef EVIL_REFLECTION_TABLE_H
#define EVIL_REFLECTION_TABLE_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Table.h"

class ReflectionTable: public Table {
public:
    ReflectionTable(): Table() {
    }

    void record_call(int call_id,
                     const char* function,
                     int eval_frame_depth = NA_INTEGER,
                     int call_frame_depth = NA_INTEGER,
                     int reverse_frame_depth = NA_INTEGER) {
        call_ids_.push_back(call_id);
        functions_.push_back(function);
        eval_frame_depths_.push_back(eval_frame_depth);
        call_frame_depths_.push_back(call_frame_depth);
        int accessed_frame_depth = reverse_frame_depth == NA_INTEGER ? NA_INTEGER : call_frame_depth - accessed_frame_depth;
        accessed_frame_depths_.push_back(accessed_frame_depth);
        int leak = NA_LOGICAL;
        if(eval_frame_depth != NA_INTEGER && call_frame_depth != NA_INTEGER && accessed_frame_depth != NA_INTEGER) {
            leak = accessed_frame_depth <= eval_frame_depth;
        }
        leaks_.push_back(leak);
    }

    SEXP to_data_frame() {

        int row_size = call_ids_.size();

        SEXP r_call_ids = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_functions = PROTECT(allocVector(STRSXP, row_size));
        SEXP r_eval_frame_depths = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_call_frame_depths = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_accessed_frame_depths = PROTECT(allocVector(INTSXP, row_size));
        SEXP r_leaks = PROTECT(allocVector(INTSXP, row_size));

        for(int row_index = 0; row_index < row_size; ++row_index) {
            INTEGER(r_call_ids)[row_index] = call_ids_[row_index];
            SET_STRING_ELT(r_functions, row_index, mkChar(functions_[row_index].c_str()));
            INTEGER(r_eval_frame_depths)[row_index] = eval_frame_depths_[row_index];
            INTEGER(r_call_frame_depths)[row_index] = call_frame_depths_[row_index];
            INTEGER(r_accessed_frame_depths)[row_index] = accessed_frame_depths_[row_index];
            LOGICAL(r_leaks)[row_index] = leaks_[row_index];
        }

        SEXP r_data_frame = create_data_frame({{"call_id", r_call_ids},
                                               {"function", r_functions},
                                               {"eval_frame_depth", r_eval_frame_depths},
                                               {"call_frame_depth", r_call_frame_depths},
                                               {"accessed_frame_depth", r_accessed_frame_depths},
                                               {"leak", r_leaks}});

        UNPROTECT(6);

        return r_data_frame;
    }

    static void inspect_and_record(ReflectionTable* table,
                                   SEXP r_call,
                                   SEXP r_rho,
                                   int call_id,
                                   int eval_frame_position,
                                   int current_frame_position);

    static SEXP get_name();

private:
    std::vector<int> call_ids_;
    std::vector<std::string> functions_;
    std::vector<int> eval_frame_depths_;
    std::vector<int> call_frame_depths_;
    std::vector<int> accessed_frame_depths_;
    std::vector<int> leaks_;
};

#endif /* EVIL_REFLECTION_TABLE_H */
