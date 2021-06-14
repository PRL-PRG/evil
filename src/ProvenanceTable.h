#ifndef EVIL_PROVENANCE_TABLE_H
#define EVIL_PROVENANCE_TABLE_H

enum class ProvenanceKind {
    parse,
    str2lang,
    str2expression,
    substitute,
    quote,
    enquote,
    call,
    as_call,
    expression,
    as_expression,
    match_call // keep it the last
};// also add as.name and as.symbol?

class ProvenanceTable: public Table {
  private:
    // Rather should be a vector of sets (or...) to handle that 
    // an expression can have several origins
    // Or one column per possible origin and then true false
    // Or the arguments if it is the origin, and NA otherwise
    // As there is one list of arguments per origin
    std::vector<std::string> parse_;
    std::vector<int> eval_ids_;
    std::vector<std::string> arguments_;
    std::vector<int> nb_provenances_;// How many provenances does it match
    std::vector<int> nb_operations_;// How many operations to build this expression
    std::vector<int> longest_path_size_;
    std::vector<std::string> provenances_; // all the roots

    // TODO: add the srcref of the provenances?
    // vector of set of arguments, provenances, provenance id
    // one column per possible type, with NA if there is not, 
    // or concatenation of the arguments
    // Column with number of different addresses (in terms of calls, not sites) for one given
    // provenance kind
    // plus column of the srcref?

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
        case ProvenanceKind::call:
            return "call";
        case ProvenanceKind::as_call:
            return "as.call";
        case ProvenanceKind::expression:
            return "expression";
        case ProvenanceKind::as_expression:
            return "as.expression";
        case ProvenanceKind::match_call:
            return "match.call";

        default:
            error("Provenance does not exist.\n");
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
        case Function::Identity::Substitute:
            return ProvenanceKind::substitute;
        case Function::Identity::Match_call:
            return ProvenanceKind::match_call;
        case Function::Identity::Expression:
            return ProvenanceKind::expression;
        case Function::Identity::AsExpression:
            return ProvenanceKind::as_expression;
        case Function::Identity::Call:
            return ProvenanceKind::call;
        case Function::Identity::AsCall:
            return ProvenanceKind::as_call;
        
        default:
            // should never happen
            // R error here
            error("Unexpected provenance!\n");
            break;
        }
    }

    void record(int eval_id, 
        const std::string& kind,
        const std::string& arguments,
        int nb_provenances,
        int nb_operations,
        int longest_path_size,
        const std::string& provenances) {
            eval_ids_.push_back(eval_id);
            parse_.push_back(kind);
            arguments_.push_back(arguments);
            nb_provenances_.push_back(nb_provenances);
            nb_operations_.push_back(nb_operations);
            longest_path_size_.push_back(longest_path_size);
            provenances_.push_back(provenances);
        }

    SEXP as_data_frame() override {
        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_ids_))},
             {"provenance", PROTECT(create_character_vector(parse_))}, 
             {"provenance_args", PROTECT(create_character_vector(arguments_))},
             {"nb_provenances", PROTECT(create_integer_vector(nb_provenances_))},
             {"nb_operations", PROTECT(create_integer_vector(nb_operations_))},
             {"longest_path_size", PROTECT(create_integer_vector(longest_path_size_))},
             {"all_provenances", PROTECT(create_character_vector(provenances_))}
             });


        UNPROTECT(7);

        return r_data_frame;
    }
};

#endif