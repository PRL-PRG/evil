#ifndef EVIL_WRITES_TABLE_H
#define EVIL_WRITES_TABLE_H

class WritesTable: public Table {
  public:
    WritesTable(): Table("writes") {
    }

    void record(int eval_call_id,
                const std::string& event,
                int transitive,
                const char* variable,
                int eval_env_depth,
                const std::string& envkind = MissingStringValue) {
        eval_call_ids_.push_back(eval_call_id);
        event_.push_back(event);
        transitive_.push_back(transitive);
        variable_.push_back(variable);
        eval_env_depth_.push_back(eval_env_depth);
        envkind_.push_back(envkind);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_call_ids_))},
             {"event", PROTECT(create_character_vector(event_))},
             {"transitive", PROTECT(create_integer_vector(transitive_))},
             {"variable", PROTECT(create_character_vector(variable_))},
             {"eval_env_depth", PROTECT(create_integer_vector(eval_env_depth_))},
             {"envkind", PROTECT(create_character_vector(envkind_))}});

        UNPROTECT(6);

        return r_data_frame;
    }

  private:
    std::vector<int> eval_call_ids_;
    std::vector<std::string> event_;
    std::vector<int> transitive_;
    std::vector<std::string> variable_;
    std::vector<int> eval_env_depth_;
    std::vector<std::string> envkind_;
};

#endif /* EVIL_WRITES_TABLE_H */
