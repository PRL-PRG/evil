#include "Environment.h"

int local_id = 0;
int foreign_id = -1;

void Environment::inc_ref(Environment* environment) {
    ++environment->ref_;
}

void Environment::dec_ref(Environment* environment) {
    --environment->ref_;
    if (environment->ref_ == 0) {
        delete environment;
    }
}

Environment* Environment::local(SEXP r_rho) {
    return new Environment(local_id++, r_rho);
}

Environment* Environment::foreign(SEXP r_rho) {
    return new Environment(foreign_id--, r_rho);
}
