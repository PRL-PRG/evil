#ifndef EVIL_PROVENANCE_H
#define EVIL_PROVENANCE_H

#include <vector>
#include <deque>
#include <memory>
#include <algorithm>
#include <numeric>
#define R_NO_REMAP
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
    // one node can be thr provenance of several nodes
    // but we do not need a destructor here as they will be reclaimed
    // in ProvenanceGraph
    std::vector<Provenance* > parents_;
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
            parents_.clear();
    }

    void add_parent(Provenance* parent) {
        parents_.push_back(parent);
    }

    const std::vector<Provenance*>& parents() const {
        return parents_;
    }

    size_t nb_parents() const {
        return parents_.size();
    }

    const std::string& get_name() const {
        return function_name_;
    }

    const std::string& get_full_call() const {
        return full_call_;
    }

    long get_id() const {
        return prov_id_;
    }

    const Provenance* get_representative() const {
        // Currently, we just take the first parent
        // Other possible strategies:
        // -  get root of longest path
        // - get smallest prov_id
        if(nb_parents() == 0) {
            return this;
        }
        else {
            return parents_[0]->get_representative();
        }
    }


    // They are actually leaves...
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

    int longest_path() const {
        int max = 0;
        for(auto parent : parents_) {
            max = std::max({max, parent->longest_path()});
        }
        return 1 + max;
    }

    size_t nb_nodes() const {
        int n = 1;
        for(auto parent : parents_) {
            n += parent->nb_nodes();
        }
        return n;
    }

};

class ProvenanceGraph {
    private:
        // We need a data structure where iterators are not 
        // invalidated after insertion at the back 
        // If we can bound up the number of nodes, we
        // can also use std::vector and reserve...
        std::deque<Provenance> provenance_nodes; 
    public:

        ProvenanceGraph() {}

        Provenance* add_node(SEXP address, std::string const& function_name, std::string const& full_call, long prov_id) {
            provenance_nodes.emplace_back(address, function_name, full_call, prov_id);
            return &provenance_nodes.back();
        }

        std::vector<Provenance*> roots() {
            std::vector<Provenance*> roots_v;
            roots_v.reserve(provenance_nodes.size());
            for(auto node: provenance_nodes) {
                if(node.nb_parents() == 0) {
                    roots_v.push_back(&node);
                }
            }
            return roots_v;
        }

        size_t nb_nodes() const {
            return provenance_nodes.size();
        }

        static void toDot(const std::string& filename, int call_id, Provenance* node);

        void toDot(const std::string& filename);

};




#endif