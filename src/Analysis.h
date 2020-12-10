#ifndef EVIL_ANALYSIS_H
#define EVIL_ANALYSIS_H

#include <vector>
#include <string>
#include <utility>
#include "r_init.h"
#include "TracerState.h"
#include "Event.h"
#include "Table.h"

class Analysis {
  public:
    Analysis() {
    }

    virtual ~Analysis() {
    }

    virtual void analyze(TracerState& tracer_state, Event& event) = 0;

    virtual std::vector<Table*> get_tables() = 0;
};

SEXP create_data_frame(
    const std::vector<std::pair<std::string, SEXP>>& columns);
SEXP create_logical_vector(const std::vector<int>& elements);
SEXP create_integer_vector(const std::vector<int>& elements);
SEXP create_double_vector(const std::vector<double>& elements);
SEXP create_character_vector(const std::vector<std::string>& elements);

#endif /* EVIL_ANALYSIS_H */
