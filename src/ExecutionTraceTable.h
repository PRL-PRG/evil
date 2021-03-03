#ifndef EVIL_EXECUTION_TRACE_TABLE_H
#define EVIL_EXECUTION_TRACE_TABLE_H

#include <vector>
#include <string>
#include "Table.h"

class ExecutionTraceTable: public Table {
  public:
    ExecutionTraceTable(): Table("trace") {
    }

    void record(int depth,
                int call_id,
                const std::string& event,
                const std::string& argument,
                int env_id,
                int receiver_eval_id,
                int parent_eval_id,
                const std::string& source) {
        depths_.push_back(depth);
        call_ids_.push_back(call_id);
        events_.push_back(event);
        arguments_.push_back(argument);
        env_ids_.push_back(env_id);
        receiver_eval_ids_.push_back(receiver_eval_id);
        parent_eval_ids_.push_back(parent_eval_id);
        sources_.push_back(source);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"depth", PROTECT(create_integer_vector(depths_))},
             {"call_id", PROTECT(create_integer_vector(call_ids_))},
             {"event", PROTECT(create_character_vector(events_))},
             {"argument", PROTECT(create_character_vector(arguments_))},
             {"env_id", PROTECT(create_integer_vector(env_ids_))},
             {"receiver_eval_id", PROTECT(create_integer_vector(receiver_eval_ids_))},
             {"parent_eval_id", PROTECT(create_integer_vector(parent_eval_ids_))},
             {"source", PROTECT(create_character_vector(sources_))}});

        UNPROTECT(8);

        return r_data_frame;
    }

  private:
    std::vector<int> depths_;
    std::vector<int> call_ids_;
    std::vector<std::string> events_;
    std::vector<std::string> arguments_;
    std::vector<int> env_ids_;
    std::vector<int> receiver_eval_ids_;
    std::vector<int> parent_eval_ids_;
    std::vector<std::string> sources_;
};

#endif /* EVIL_EXECUTION_TRACE_TABLE_H */
