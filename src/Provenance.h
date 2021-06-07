#ifndef EVIL_PROVENANCE_H
#define EVIL_PROVENANCE_H

#include <vector>
#include <memory>
#include <algorithm>
#include <numeric>
#include "r_init.h"

/**
  Models the graph of origins of an expression to eval.

  Given:

  x <- f(y)

  If x is of interest (LANGSXP, EXPRSXP, SYMSXP,
  or some function of interest), we record it. We also look
  whether y (or its elements) is already recorded.
  If yes, we can affect y (or the elements of y) as the
  parents of x.
 */

class Provenance {
  private:
    // SEXP or pointer to the provenance?
    // For instance, a SEXP could be reclaimed
    // but we still want to keep track of it
    // Or only one parent, because we won't bother looking into a VECSXP?
    std::vector<Provenance*> parents_;
    SEXP address_;
    std::string function_name_;
    std::string full_call_;
    long prov_id_; // unique id of the call creating this provenance

  public:
    Provenance(SEXP address,
               std::string function_name,
               std::string full_call,
               long prov_id)
        : address_(address)
        , function_name_(function_name)
        , full_call_(full_call)
        , prov_id_(prov_id) {
    }

    void add_parent(Provenance* parent) {
        parents_.push_back(parent);
    }

    std::vector<Provenance*>& parents() const {
        return parents_;
    }

    size_t nb_parents() const {
        return parents_.size();
    }

    char const* get_name() const {
        return function_name_;
    }

    Provenance const* get_representative() const {
        // Currently, we just take the first parent
        // Other possible strategies:
        // -  get root of longest path
        // - get smallest prov_id
        if(nb_parents() == 0) {
            return this;
        }
        else {
            return parents_[0];
        }
    }

    size_t nb_roots() const {
        if (nb_parents() == 0) {
            return 1;
        }
        else {
            int sum = 0;
            for(auto parent : parents_) {
                sum += parent->nb_roots();
            }
            return sum;
        }
    }

    size_t longest_path() const {
        if(nb_parents() == 0) {
            return 1;
        }
        else {
            int max = -1;
            for(auto parent : parents_) {
                max = std::max(max, parent->longest_path());
            }
            return 1 + max;
        }
    }


    ~Provenance() {
        // A node also removes its parents
        for (auto parent: parents_) {
            delete parent;
        }
    }
}

class ProvenanceGraph {
    private:
        std::vector<Provenance> provenance_nodes;

    public:
        ProvenanceGraph(int nb_nodes) {
            provenance_nodes.reserve(nb_nodes);
        }

        ProvenanceGraph() {}

        Provenance* add_node(SEXP address, std::string function_name, std::string full_call, long prov_id) {
            provenance_nodes.emplace_back(address, function_name, full_call, prov_id);
            return &provenance_nodes.back();
        }

        std::vector<Provenance*> roots() {
            std::vector<Provenance*> roots_v;
            roots_v.reserve(provenance_nodes.size());
            remove_copy_if(provenance_nodes.begin(), provenance_nodes.end(), 
                roots_v,
                [](Provenance* n) {return nb_parents() != 0; } )
        }

};

#endif