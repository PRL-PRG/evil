#ifndef EVIL_PROVENANCE_ANALYSIS_H
#define EVIL_PROVENANCE_ANALYSIS_H

#include <vector>
#include <string>
#include "r_init.h"
#include "Analysis.h"
#include "ProvenanceTable.h"

class ProvenanceAnalysis: public Analysis {
    private:
        ProvenanceTable provenance_table_;
    public:
        ProvenanceKind(): Analysis() {}

        void analyze(TracerState& tracer_state, Event& event) override {
            Event::Type event_type = event.get_type();
            Stack& stack = tracer_state.get_stack();

            if(event_type == Event::Type::ClosureCallExit) {
                const StackFrame& frame = stack.peek();
                const Call* call = frame.as_call();
                const Function* function = call->get_function();

                if(function->has_identity(Function::Identity::ProvenanceFamily)) {
                    // Get the return value

                    // Get address of the value or of the elements if it is a complex expression
                    // such as a list

                    // Add mapping(s)
                    // address -> Identity (or directly ProvenanceKind)
                }

                if(function->has_identity(Function::Identity::EvalFamily)) {
                    // Check if the expression address contains any address saved previously.

                    // if yes, record

                    // TODO: can be potentially tagged with several 
                }
            }

            // Probably add a hook for GC desallocation, to remove for our table

        }

};

#endif