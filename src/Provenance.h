#ifndef EVIL_PROVENANCE_H
#define EVIL_PROVENANCE_H


#include <vector>
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
        std::vector<SEXP> parents_;
        SEXP address_;
        std::string function_name_;
        std::string full_call_;
        long prov_id_; // unique id of the call creating this provenance

    public:
        Provenance(SEXP address, const std:string& function_name, const std::string& full_call, long prov_id) : 
        address_(address), function_name_(function_name), full_call_(full_call), prov_id_(prov_id) { }

    void add_parent(SEXP parent) {
        parents_.push_back(parent);
    }
}


#endif