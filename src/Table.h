#ifndef EVIL_TABLE_H
#define EVIL_TABLE_H

#include <vector>
#include <string>
#include <utility>
#include "r_init.h"
#include "CallState.h"

class Table {
public:
    Table() {
    }

    virtual ~Table() {
    }

    virtual void inspect_and_record(CallState& call_state) = 0;

    virtual SEXP to_data_frame() = 0;
};

SEXP create_data_frame(const std::vector<std::pair<std::string, SEXP>>& columns);
SEXP create_logical_vector(const std::vector<int>& elements);
SEXP create_integer_vector(const std::vector<int>& elements);
SEXP create_double_vector(const std::vector<double>& elements);
SEXP create_character_vector(const std::vector<std::string>& elements);

#endif /* EVIL_TABLE_H */
