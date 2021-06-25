#ifndef EVIL_PROVENANCE_H
#define EVIL_PROVENANCE_H

#include <vector>
#include <memory>
#include <algorithm>
#include <unordered_set>
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
    int rep_parent = 0;
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

    void add_parent(Provenance* parent, bool result = false) {
        // We don't insert parents of functions of interests
        // Otherwise, for instance, we would get { and >internal(parse(...)) for parse
        if(prov_functions.find(function_name_) != prov_functions.end()) {
            return;
        }
        parents_.push_back(parent);

        // if we picked this parent out because it was a return address
        // It happens to be the LHS for assignment functions
        // so this is the one we want when climbing up for a representative
        if(result) {
            rep_parent = parents_.size() - 1;
        }
    }

    const std::vector<Provenance*>& parents() const {
        return parents_;
    }

    size_t nb_parents() const {
        return parents_.size();
    }

    const SEXP get_address() const {
        return address_;
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
            return parents_[rep_parent]->get_representative();
        }
    }

    // Shows the full calls of the provenances on the path
    // up to the representative
    // TODO? Compress the path by showing only once consecutive
    // similar calls and the number of times
    std::pair<std::string, int> rep_path() const {
        std::string path;
        int path_length = get_rep_aux(path);

        return std::make_pair(path, path_length);
    }

    int repr_path_size() const {
        if(nb_parents() > 0) {
            1 + parents_[rep_parent]->repr_path_size();
        }
        return 1;
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

    void roots(std::unordered_set<std::string>& unique_provs) const {
        if(nb_parents() == 0) {
            unique_provs.insert(this->function_name_);
        }
        else {
            for(auto parent : parents_) {
                parent->roots(unique_provs);
            }
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

    inline static std::unordered_set<std::string> prov_functions = {
        "parse", "str2lang", "str2expression", "substitute", "quote", "enquote", "match.call",
        "call", "as.call", "expression", "as.expression", "as.name", "as.symbol", "formals"
    };

    inline static size_t nb_special_functions() {return Provenance::prov_functions.size();}

    private:

    int get_rep_aux(std::string& path) const {
        path += "|" + this->get_full_call() + "|";
        if(nb_parents() > 0) {
            path += " -> ";
            return 1 + parents_[rep_parent]->get_rep_aux(path);
        }
        return 1;
    }

};

class ProvenanceGraph {
    private:
        std::vector<Provenance*> provenance_nodes; 
    public:

        ProvenanceGraph() {
            // There are at least a few nodes and it seems gcc only reserve for one node on Linux
            // 1643 nodes after testing on one file for instance
            // But reserving 2000 nodes seems to slow things down
            // Even 4 slows down
            //provenance_nodes.reserve(4);
        }

        Provenance* add_node(SEXP address, std::string const& function_name, std::string const& full_call, long prov_id) {
            provenance_nodes.push_back(new Provenance(address, function_name, full_call, prov_id));
            return provenance_nodes.back();
        }

        Provenance* add_node(const Provenance& p) {
            Provenance* prov = new Provenance(p.get_address(), p.get_name(),
             p.get_full_call(), p.get_id());
            
            for(auto parent : p.parents()) {
                prov->add_parent(parent);
            }
            provenance_nodes.push_back(prov);
            return prov;
        }

        std::vector<Provenance*> roots() {
            std::vector<Provenance*> roots_v;
            roots_v.reserve(provenance_nodes.size());
            for(auto node: provenance_nodes) {
                if(node->nb_parents() == 0) {
                    roots_v.push_back(node);
                }
            }
            return roots_v;
        }

        size_t nb_nodes() const {
            return provenance_nodes.size();
        }

        static void toDot(const std::string& filename, int call_id, Provenance* node);

        void toDot(const std::string& filename);

        ~ProvenanceGraph() {
            for(auto node : provenance_nodes) {
                delete node;
            }
        }
    
};




#endif