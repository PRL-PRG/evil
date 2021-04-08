#ifndef EVIL_WRITES_TABLE_H
#define EVIL_WRITES_TABLE_H

class WritesTable: public Table {
  public:
    WritesTable(): Table("writes") {
    }

    void record(int eval_id,
                const std::string& event,
                int transitive,
                const std::string& variable,
                int env_id,
                int parent_eval_id,
                int receiver_eval_id,
                const std::string& source,
                int in_envir) {
        eval_ids_.push_back(eval_id);
        event_.push_back(event);
        transitive_.push_back(transitive);
        variable_.push_back(variable);
        env_ids_.push_back(env_id);
        parent_eval_id_.push_back(parent_eval_id);
        receiver_eval_id_.push_back(receiver_eval_id);
        source_.push_back(source);
        in_envir_.push_back(in_envir);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_id", PROTECT(create_integer_vector(eval_ids_))},
             {"event", PROTECT(create_character_vector(event_))},
             {"transitive", PROTECT(create_integer_vector(transitive_))},
             {"variable", PROTECT(create_character_vector(variable_))},
             {"env_id", PROTECT(create_integer_vector(env_ids_))},
             {"parent_eval_id", PROTECT(create_integer_vector(parent_eval_id_))},
             {"receiver_eval_id", PROTECT(create_integer_vector(receiver_eval_id_))},
             {"source", PROTECT(create_character_vector(source_))},
             {"in_envir_", PROTECT(create_integer_vector(in_envir_))}});

        UNPROTECT(9);

        return r_data_frame;
    }

  private:
    std::vector<int> eval_ids_;
    std::vector<std::string> event_;
    std::vector<int> transitive_;
    std::vector<std::string> variable_;
    std::vector<int> env_ids_;
    std::vector<int> parent_eval_id_;
    std::vector<int> receiver_eval_id_;
    std::vector<std::string> source_;
    std::vector<int> in_envir_;
};

#endif /* EVIL_WRITES_TABLE_H */
