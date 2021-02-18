#ifndef PROMISEDYNTRACER_STACK_H
#define PROMISEDYNTRACER_STACK_H

#include "StackFrame.h"

#include <vector>

using stack_frames_t = std::vector<StackFrame>;

class Stack {
  public:
    using iterator = stack_frames_t::iterator;
    using reverse_iterator = stack_frames_t::reverse_iterator;
    using const_iterator = stack_frames_t::const_iterator;
    using const_reverse_iterator = stack_frames_t::const_reverse_iterator;

    explicit Stack(): stack_() {
    }

    size_t size() const {
        return stack_.size();
    }

    bool is_empty() const {
        return stack_.empty();
    }

    iterator begin() {
        return stack_.begin();
    }

    iterator end() {
        return stack_.end();
    }

    reverse_iterator rbegin() {
        return stack_.rbegin();
    }

    reverse_iterator rend() {
        return stack_.rend();
    }

    const_iterator cbegin() const {
        return stack_.cbegin();
    }

    const_iterator cend() const {
        return stack_.cend();
    }

    const_reverse_iterator crbegin() const {
        return stack_.crbegin();
    }

    const_reverse_iterator crend() const {
        return stack_.crend();
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
        return stack_.at(stack_.size() - n);
    }

    StackFrame& peek(std::size_t n = 1) {
        return stack_.at(stack_.size() - n);
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
