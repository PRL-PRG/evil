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
    std::vector<std::string> eval_function_;
    std::vector<std::string> arguments_;
    std::vector<int> nb_provenances_;// How many provenances does it match

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
        const std::string& eval_function,
        const std::string& kind,
        const std::string& arguments,
        int nb_provenances) {
            eval_ids_.push_back(eval_id);
            parse_.push_back(kind);
            eval_function_.push_back(eval_function);
            arguments_.push_back(arguments);
            nb_provenances_.push_back(nb_provenances);
        }

    SEXP as_data_frame() override {
        /*std::vector<std::string> provenance_strs(parse_.size());
        for(int i = 0; i < parse_.size(); i ++) {
            provenance_strs[i] = provenance_to_string(parse_[i]);
        }*/

        SEXP r_data_frame = create_data_frame(
            {{"eval_id", PROTECT(create_integer_vector(eval_ids_))},
             {"eval_function", PROTECT(create_character_vector(eval_function_))}, 
             {"provenance", PROTECT(create_character_vector(parse_))}, 
             {"provenance_args", PROTECT(create_character_vector(arguments_))},
             {"nb_provenances", PROTECT(create_integer_vector(nb_provenances_))}});


        UNPROTECT(5);

        return r_data_frame;
    }
};

#endif