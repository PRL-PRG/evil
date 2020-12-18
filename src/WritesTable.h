#ifndef EVIL_WRITES_TABLE_H
#define EVIL_WRITES_TABLE_H

class WritesTable: public Table {
  public:
    WritesTable(): Table("writes") {
    }

    void record(int eval_call_id,
                const std::string& category,
                const char* variable,
                int local = NA_LOGICAL,
                const std::string& envkind = MissingStringValue) {
        eval_call_ids_.push_back(eval_call_id);
        category_.push_back(category);
        variable_.push_back(variable);
        local_.push_back(local);
        envkind_.push_back(envkind);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_call_ids_))},
             {"category", PROTECT(create_character_vector(category_))},
             {"variable", PROTECT(create_character_vector(variable_))},
             {"local", PROTECT(create_logical_vector(local_))},
             {"envkind", PROTECT(create_character_vector(envkind_))}});

        UNPROTECT(5);

        return r_data_frame;
    }

  private:
    std::vector<int> eval_call_ids_;
    std::vector<std::string> category_;
    std::vector<std::string> variable_;
    std::vector<int> local_;
    std::vector<std::string> envkind_;
};

#endif /* EVIL_WRITES_TABLE_H */
