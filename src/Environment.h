#ifndef EVIL_ENVIRONMENT_H
#define EVIL_ENVIRONMENT_H

#include <string>
#include "Call.h"

class Environment {
  public:
    enum class Source { Unknown, Package, Call, Explicit };

    int get_id() const {
        return id_;
    }

    SEXP get_env() {
        return r_rho_;
    }

    Source get_source() const {
        return source_;
    }

    Call* get_call() const {
        return call_;
    }

    void set_call_source(Call* call) {
        set_source_(Source::Call, call);
    }

    void set_explicit_source(Call* call) {
        set_source_(Source::Explicit, call);
    }

    bool is_eval_argument() {
        return receiver_eval_id_;
    }

    int get_receiver_eval_id() const {
        return receiver_eval_id_;
    }

    void set_receiver_eval_id(int receiver_eval_id) {
        receiver_eval_id_ = receiver_eval_id;
    }

    int get_parent_eval_id() const {
        return parent_eval_id_;
    }

    void set_parent_eval_id(int parent_eval_id) {
        parent_eval_id_ = parent_eval_id;
    }

    std::string get_formatted_source() {
        switch (get_source()) {
        case Source::Unknown:
            return "???";
        case Source::Package:
            return "package:";
        case Source::Explicit:
            return "explicit:" +
                   get_call()->get_function()->get_qualified_name();
        case Source::Call:
            return "call:" + get_call()->get_function()->get_qualified_name();
        }
    }

    static void inc_ref(Environment* environment);

    static void dec_ref(Environment* environment);

    static int get_next_id();

    static Environment* local(SEXP r_rho);

    static Environment* foreign(SEXP r_rho);

  private:
    const int id_;
    SEXP r_rho_;
    Source source_;
    Call* call_;
    int receiver_eval_id_;
    int parent_eval_id_;
    int ref_;

    Environment(int id, SEXP r_rho)
        : id_(id)
        , r_rho_(r_rho)
        , source_(Source::Unknown)
        , call_(nullptr)
        , parent_eval_id_(0)
        , receiver_eval_id_(0)
        , ref_(1) {
        // const char* name = get_package_name_(r_rho);
        //
        // if (name != nullptr) {
        //    source_ = Source::Package;
        //    package_name_ = name;
        //}
    }

    ~Environment() {
        if (call_ != nullptr) {
            Call::dec_ref(call_);
        }
    }

    void set_source_(Source source, Call* call) {
        /* if source is known then don't reset */
        if (source_ != Source::Unknown) {
            return;
        }

        source_ = source;

        if (call == nullptr) {
            Rf_error("attempt to set Call as environment source with an "
                     "invalid call reference");
        }
        call_ = call;
        Call::inc_ref(call_);
    }
};

#endif /* EVIL_ENVIRONMENT_H */
