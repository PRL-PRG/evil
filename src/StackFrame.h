#ifndef EVIL_STACK_FRAME_H
#define EVIL_STACK_FRAME_H

#include "Call.h"

class StackFrame {
  public:
    enum class Type { Call, Context };

    ~StackFrame() {
    }

    static StackFrame from_call(Call* call) {
        return StackFrame(call, Type::Call);
    }

    static StackFrame from_context(void* context) {
        return StackFrame(context, Type::Context);
    }

    bool is_call() const {
        return (type_ == Type::Call);
    }

    Call* as_call() {
        return call_;
    }

    const Call* as_call() const {
        return call_;
    }

    bool is_context() const {
        return (type_ == Type::Context);
    }

    void* as_context() {
        return context_;
    }

    const void* as_context() const {
        return context_;
    }

  private:
    Type type_;
    union {
        Call* call_;
        void* context_;
    };

    StackFrame(void* ptr, Type type): type_(type) {
        switch (type) {
        case Type::Call:
            call_ = (Call*) ptr;
            break;
        case Type::Context:
            context_ = ptr;
            break;
        }
    }
};

#endif /* EVIL_STACK_FRAME_H */
