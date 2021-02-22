#include "Environment.h"

int id = 0;
int Environment::get_next_id() {
    return id++;
}

void Environment::inc_ref(Environment* environment) {
    ++environment->ref_;
}

void Environment::dec_ref(Environment* environment) {
    --environment->ref_;
    if (environment->ref_ == 0) {
        delete environment;
    }
}
