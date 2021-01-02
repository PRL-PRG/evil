#ifndef EVIL_REFLECTION_TABLE_H
#define EVIL_REFLECTION_TABLE_H

class ReflectionTable: public Table {
  public:
    ReflectionTable(): Table("reflection") {
    }

    void record(int eval_call_id,
                const char* function,
                int eval_frame_depth,
                int current_frame_depth,
                int accessed_frame_depth) {

        eval_call_ids_.push_back(eval_call_id);
        functions_.push_back(function);
        eval_frame_depths_.push_back(eval_frame_depth);
        current_frame_depths_.push_back(current_frame_depth);
        accessed_frame_depths_.push_back(accessed_frame_depth);
    }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_call_ids_))},
             {"function", PROTECT(create_character_vector(functions_))},
             {"eval_frame_depth",
              PROTECT(create_integer_vector(eval_frame_depths_))},
             {"current_frame_depth",
              PROTECT(create_integer_vector(current_frame_depths_))},
             {"accessed_frame_depth",
              PROTECT(create_integer_vector(accessed_frame_depths_))}});

        UNPROTECT(5);

        return r_data_frame;
    }

  private:
    std::vector<int> eval_call_ids_;
    std::vector<std::string> functions_;
    std::vector<int> eval_frame_depths_;
    std::vector<int> current_frame_depths_;
    std::vector<int> accessed_frame_depths_;
};

#endif /* EVIL_REFLECTION_TABLE_H */
