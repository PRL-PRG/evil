#ifndef EVIL_TABLE_H
#define EVIL_TABLE_H

#include <vector>
#include <string>
#include <utility>
#include "r_init.h"

class Table {
public:
    Table() {
    }

    virtual ~Table() {
    }

    virtual SEXP to_data_frame() = 0;
};

SEXP create_data_frame(const std::vector<std::pair<std::string, SEXP>>& columns);

#endif /* EVIL_TABLE_H */
