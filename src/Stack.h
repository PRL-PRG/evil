#ifndef PROMISEDYNTRACER_STACK_H
#define PROMISEDYNTRACER_STACK_H

#include "StackFrame.h"

#include <vector>

using stack_frames_t = std::vector<StackFrame>;

class Stack {
  public:
    explicit Stack(): stack_() {
    }

    size_t size() const {
        return stack_.size();
    }

    bool is_empty() const {
        return stack_.empty();
    }

    void push(StackFrame& frame) {
        stack_.push_back(frame);
    }

    StackFrame pop() {
        StackFrame frame{peek(0)};
        stack_.pop_back();
        return frame;
    }

    StackFrame& peek(std::size_t n = 0,
                     Function::Type type = Function::Type::Any) {
        StackFrame& frame = stack_[stack_.size() - n - 1];
        return frame;
    }

    const StackFrame& peek(std::size_t n = 0,
                           Function::Type type = Function::Type::Any) const {
        const StackFrame& frame = stack_[stack_.size() - n - 1];
        return frame;
    }

    Call* peek_call(std::size_t n = 0,
                    Function::Type type = Function::Type::Any) {
        int index = get_call_index_(n, type);

        if (index < 0)
            return nullptr;

        StackFrame& frame = stack_[index];
        return frame.as_call();
    }

    int count_call(Function::Type type = Function::Type::Any) {
        int count = 0;

        for (int index = stack_.size() - 1; index >= 0; --index) {
            const StackFrame& frame = stack_[index];
            if (frame.is_call() &&
                frame.as_call()->get_function()->has_type(type)) {
                ++count;
            }
        }

        return count;
    }

  private:
    stack_frames_t stack_;

    int get_call_index_(std::size_t n = 0,
                        Function::Type type = Function::Type::Any) const {
        for (int index = stack_.size() - 1; index >= 0; --index) {
            const StackFrame& frame = stack_[index];
            if (frame.is_call() &&
                frame.as_call()->get_function()->has_type(type)) {
                if (n == 0) {
                    return index;
                }
                --n;
            }
        }

        return -1;
    }
};

#endif /* PROMISEDYNTRACER_EXECUTION_CONTEXT_STACK_H */
