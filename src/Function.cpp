#include "Function.h"

void Function::inc_ref(Function* function) {
    ++function->ref_;
}

void Function::dec_ref(Function* function) {
    --function->ref_;
    if (function->ref_ == 0) {
        delete function;
    }
}
