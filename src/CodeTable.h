#ifndef EVIL_CODE_TABLE_H
#define EVIL_CODE_TABLE_H

class CodeTable: public Table {
  public:
    CodeTable(): Table("code") {
    }

    void record(int call_id,
                const std::string& function,
                const std::string& description,
                int local = 0) {
        call_ids_.push_back(call_id);
        functions_.push_back(function);
        descriptions_.push_back(description);
        locals_.push_back(local);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(call_ids_))},
             {"function", PROTECT(create_character_vector(functions_))},
             {"description", PROTECT(create_character_vector(descriptions_))},
             {"local", PROTECT(create_logical_vector(locals_))}});

        UNPROTECT(4);

        return r_data_frame;
    }

  private:
    std::vector<int> call_ids_;
    std::vector<std::string> functions_;
    std::vector<std::string> descriptions_;
    std::vector<int> locals_;
};

#endif /* EVIL_CODE_TABLE_H */
