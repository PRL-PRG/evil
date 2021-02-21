#include "Call.h"

void Call::inc_ref(Call* call) {
    ++call->ref_;
}

void Call::dec_ref(Call* call) {
    --call->ref_;
    if (call->ref_ == 0) {
        delete call;
    }
}
