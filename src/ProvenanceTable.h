#ifndef EVIL_PROVENANCE_TABLE_H
#define EVIL_PROVENANCE_TABLE_H

enum class ProvenanceKind {
    parse,
    str2lang,
    str2expression,
    substitute,
    quote,
    enquote,
    match_call
};

class ProvenanceTable: public Table {
  private:
    std::vector<ProvenanceKind> parse_;
    std::vector<int> eval_ids_;
    std::vector<std::string> arguments_;

  public:
    ProvenanceTable(): Table("provenances") {
    }

    static const char* provenance_to_string(ProvenanceKind kind) {
        switch (kind) {
        case ProvenanceKind::parse:
            return "parse";
        case ProvenanceKind::str2lang:
            return "str2lang";
        case ProvenanceKind::str2expression:
            return "str2expression";
        case ProvenanceKind::substitute:
            return "substitute";
        case ProvenanceKind::quote:
            return "quote";
        case ProvenanceKind::enquote:
            return "enquote";
        case ProvenanceKind::match_call:
            return "match.call";
        }
    }

    void record(int eval_id, 
        ProvenanceKind kind,
        const std::string& arguments) {
            eval_ids_.push_back(eval_id);
            parse_.push_back(kind);
            arguments_.push_back(arguments);
        }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_id", PROTECT(create_integer_vector(eval_ids_))},
             {"provenance",
              PROTECT(create_integer_vector(
                  parse_))}, // probably convert to string here already?
             {"provenance_args", PROTECT(create_character_vector(arguments_))}});
        UNPROTECT(2);

        return r_data_frame;
    }
};

#endif