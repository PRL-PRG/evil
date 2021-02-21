#include "Environment.h"

void Environment::inc_ref(Environment* environment) {
    ++environment->ref_;
}

void Environment::dec_ref(Environment* environment) {
    --environment->ref_;
    if (environment->ref_ == 0) {
        delete environment;
    }
}
