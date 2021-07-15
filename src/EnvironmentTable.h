#ifndef EVIL_ENVIRONMENT_TABLE_H
#define EVIL_ENVIRONMENT_TABLE_H

#include <R.h>
#include <Rinternals.h>
#include "Environment.h"
#include "robin_hood.h"

class EnvironmentTable {
  public:
    EnvironmentTable() {
    }

    ~EnvironmentTable() {
        for (auto& it: table_) {
            Environment::dec_ref(it.second);
        }
    }

    void initialize() {
    }

    Environment* insert(SEXP r_rho) {
        Environment* environment = Environment::local(r_rho);

        auto result = table_.insert({r_rho, environment});

        if (!result.second) {
            Environment::dec_ref(result.first->second);
            result.first->second = environment;
        }

        return environment;
    }

    void remove(SEXP r_environment) {
        auto result = table_.find(r_environment);

        if (result != table_.end()) {
            Environment* environment = result->second;
            table_.erase(result);
            Environment::dec_ref(environment);
        }
    }

    Environment* lookup(SEXP r_environment) {
        return get_or_create_(r_environment);
    }

  private:
    robin_hood::unordered_map<SEXP, Environment*> table_;

    Environment* get_or_create_(SEXP r_environment) {
        auto result = table_.find(r_environment);

        if (result != table_.end()) {
            return result->second;
        } else {
            Environment* environment = Environment::foreign(r_environment);
            auto result = table_.insert({r_environment, environment});
            return environment;
        }
    }
};

#endif /* EVIL_ENVIRONMENT_TABLE_H */
