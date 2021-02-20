#ifndef EVIL_EXECUTION_TRACE_TABLE_H
#define EVIL_EXECUTION_TRACE_TABLE_H

#include <vector>
#include <string>
#include "Table.h"

class ExecutionTraceTable: public Table {
  public:
    ExecutionTraceTable(): Table("trace") {
    }

    void
    record(int depth, const std::string& event, const std::string& argument) {
        depths_.push_back(depth);
        events_.push_back(event);
        arguments_.push_back(argument);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"depth", PROTECT(create_integer_vector(depths_))},
             {"event", PROTECT(create_character_vector(events_))},
             {"argument", PROTECT(create_character_vector(arguments_))}});

        UNPROTECT(3);

        return r_data_frame;
    }

  private:
    std::vector<int> depths_;
    std::vector<std::string> events_;
    std::vector<std::string> arguments_;
};

#endif /* EVIL_EXECUTION_TRACE_TABLE_H */
