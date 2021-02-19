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
        StackFrame frame{peek(1)};
        stack_.pop_back();
        return frame;
    }

    const StackFrame& peek(std::size_t n = 1) const {
        return stack_[stack_.size() - n];
    }

    StackFrame& peek(std::size_t n = 1) {
        return stack_[stack_.size() - n];
    }

    stack_frames_t unwind(void* context) {
        stack_frames_t unwound_frames;

        while (size() > 0) {
            StackFrame& temp_frame = stack_.back();

            if (temp_frame.is_context() &&
                (temp_frame.as_context() == context)) {
                return unwound_frames;
            }
            stack_.pop_back();
            unwound_frames.push_back(temp_frame);
        }
        Rf_error("cannot find matching context while unwinding\n");
    }

  private:
    stack_frames_t stack_;
};

#endif /* PROMISEDYNTRACER_EXECUTION_CONTEXT_STACK_H */
