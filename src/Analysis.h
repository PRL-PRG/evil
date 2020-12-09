#ifndef EVIL_ANALYSIS_H
#define EVIL_ANALYSIS_H

#include <vector>
#include <string>
#include <utility>
#include "r_init.h"
#include "CallState.h"

struct table_t {
    std::string name;
    SEXP r_data_frame;
};

class Analysis {
  public:
    Analysis() {
    }

    virtual ~Analysis() {
    }

    virtual void analyze(CallState& call_state) = 0;

    virtual std::vector<table_t> get_tables() = 0;
};

SEXP create_data_frame(
    const std::vector<std::pair<std::string, SEXP>>& columns);
SEXP create_logical_vector(const std::vector<int>& elements);
SEXP create_integer_vector(const std::vector<int>& elements);
SEXP create_double_vector(const std::vector<double>& elements);
SEXP create_character_vector(const std::vector<std::string>& elements);

#endif /* EVIL_ANALYSIS_H */
