#ifndef EVIL_LOOKUP_TABLE_H
#define EVIL_LOOKUP_TABLE_H

#include "Table.h"
#include <string>
#include <unordered_map>

struct LookupTableKey {
    int eval_call_id;
    bool direct;
    bool local;
    std::string envkind;
    std::string variable;
    std::string valuetype;

    LookupTableKey(int eval_call_id,
                   bool direct,
                   bool local,
                   const std::string& envkind,
                   const std::string& variable,
                   const std::string& valuetype) {
        this->eval_call_id = eval_call_id;
        this->direct = direct;
        this->local = local;
        this->envkind = envkind;
        this->variable = variable;
        this->valuetype = valuetype;
    }

    bool operator==(const LookupTableKey& other) const {
        return (other.eval_call_id == eval_call_id) &&
               (other.direct == direct) && (other.local == local) &&
               (other.envkind == envkind) && (other.variable == variable);
    }
};

template <class T>
inline void hash_combine(std::size_t& s, const T& v) {
    std::hash<T> h;
    s ^= h(v) + 0x9e3779b9 + (s << 6) + (s >> 2);
}

struct hash_fn {
    std::size_t operator()(const LookupTableKey& key) const {
        std::size_t hash = 0;
        hash_combine(hash, key.eval_call_id);
        hash_combine(hash, key.direct);
        hash_combine(hash, key.local);
        hash_combine(hash, key.envkind);
        hash_combine(hash, key.variable);
        hash_combine(hash, key.valuetype);
        return hash;
    }
};

class LookupTable: public Table {
  public:
    LookupTable(): Table("lookups") {
    }

    void record(int eval_call_id,
                bool direct,
                int local,
                std::string envkind,
                std::string variable,
                std::string valuetype) {

        /* There are 4 cases:
         * - non-eval lookups (summarized)
         * - local eval lookups (summarized)
         * - non-local eval lookups
         * -     package functions (summarized)
         * -     others
         */

        /* non-eval lookups are summarized */
        if (eval_call_id == 0) {
            direct = NA_LOGICAL;
            local = NA_INTEGER;
            variable = MissingStringValue;
            envkind = MissingStringValue;
            valuetype = MissingStringValue;
        }
        /* local eval lookups are summarized */
        else if (local == 1) {
            direct = NA_LOGICAL;
            variable = MissingStringValue;
            envkind = MissingStringValue;
            valuetype = MissingStringValue;
        }
        /* non-local */
        else if (is_package_environment_(envkind) &&
                   (valuetype == "closure" || valuetype == "builtin" ||
                    valuetype == "special")) {
            variable = MissingStringValue;
        }

        LookupTableKey key(
            eval_call_id, direct, local, envkind, variable, valuetype);

        auto result = summary_.find(key);
        if (result == summary_.end()) {
            summary_.insert({key, 1});
        } else {
            ++result->second;
        }
    }

    SEXP as_data_frame() override {
        std::vector<int> eval_call_ids;
        std::vector<int> directs;
        std::vector<int> locals;
        std::vector<std::string> envkinds;
        std::vector<std::string> variables;
        std::vector<std::string> valuetypes;
        std::vector<int> counts;

        for (const auto& it: summary_) {
            const LookupTableKey& key = it.first;
            eval_call_ids.push_back(key.eval_call_id);
            directs.push_back(key.direct);
            locals.push_back(key.local);
            variables.push_back(key.variable);
            envkinds.push_back(key.envkind);
            valuetypes.push_back(key.valuetype);
            counts.push_back(it.second);
        }

        SEXP r_data_frame = create_data_frame(
            {{"eval_call_id", PROTECT(create_integer_vector(eval_call_ids))},
             {"direct", PROTECT(create_logical_vector(directs))},
             {"local", PROTECT(create_integer_vector(locals))},
             {"variable", PROTECT(create_character_vector(variables))},
             {"envkind", PROTECT(create_character_vector(envkinds))},
             {"valuetype", PROTECT(create_character_vector(valuetypes))},
             {"count", PROTECT(create_integer_vector(counts))}});

        UNPROTECT(7);

        return r_data_frame;
    }

  private:
    std::unordered_map<LookupTableKey, int, hash_fn> summary_;

    bool is_package_environment_(const std::string& envkind) {
        return !envkind.empty() && envkind[0] != '<';
    }
};

#endif /* EVIL_LOOKUP_TABLE_H */
