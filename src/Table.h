#ifndef EVIL_TABLE_H
#define EVIL_TABLE_H

class Table {
  public:
    Table(const std::string& name): name_(name) {
    }

    virtual ~Table() {
    }

    const std::string& get_name() const {
        return name_;
    }

    virtual SEXP as_data_frame() = 0;

  private:
    std::string name_;
};

#endif /* EVIL_TABLE_H */
