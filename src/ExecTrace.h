#ifndef EVIL_EXEC_TRACE_H
#define EVIL_EXEC_TRACE_H

#undef length
#include <vector>
#include <string>
#include <fstream>

class ExecTrace {
public:
    ExecTrace() : depth_(0) {
    }

    void enter_call(const char* name) {
        add_trace_(std::string("enter ") + name);
        ++depth_;
    }

    void exit_call(const char* name) {
        --depth_;
        add_trace_(std::string("exit ") + name);
    }

    void write_var(const char* sym_name) {
        add_trace_(std::string("write ") + sym_name);
    }

    void serialize(const char* filename) {
        std::ofstream file(filename);

        for(const std::string& line: trace_) {
            file << line << std::endl;
        }

        file.close();
    }

private:

    void add_trace_(const std::string& content) {
        trace_.push_back(get_indentation_() + content);
    }

    std::string get_indentation_() {
        return std::string(depth_ * 2, ' ');
    }

    int depth_;
    std::vector<std::string> trace_;
};

#endif /* EVIL_EXEC_TRACE_H */
