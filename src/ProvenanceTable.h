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
    // Rather should be a vector of sets (or...) to handle that 
    // an expression can have several origins
    // Or one column per possible origin and then true false
    // Or the arguments if it is the origin, and NA otherwise
    // As there is one list of arguments per origin
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

    static const ProvenanceKind identity_to_provenance(Function::Identity identity) {
        switch (identity)
        {
        case Function::Identity::Enquote :
            return ProvenanceKind::enquote;
        case Function::Identity::Quote:
            return ProvenanceKind::quote;
        case Function::Identity::Parse:
            return ProvenanceKind::parse;
        case Function::Identity::Str2expression:
            return ProvenanceKind::str2expression;
        case Function::Identity::Str2lang:
            return ProvenanceKind::str2lang;
        
        default:
            // should never happen
            // R error here
            break;
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
        std::vector<std::string> provenance_strs(parse_.size());
        for(int i = 0; i < parse_.size(); i ++) {
            provenance_strs[i] = provenance_to_string(parse_[i]);
        }

        SEXP r_data_frame = create_data_frame(
            {{"eval_id", PROTECT(create_integer_vector(eval_ids_))},
             {"provenance", PROTECT(create_character_vector(provenance_strs))}, 
             {"provenance_args", PROTECT(create_character_vector(arguments_))}});


        UNPROTECT(3);

        return r_data_frame;
    }
};

#endif