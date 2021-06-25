#include "Provenance.h"


#include <iostream>
#include <fstream>

void ProvenanceGraph::toDot(const std::string& filename) {
    //TODO
}

// Inspired by https://stackoverflow.com/questions/2417588/escaping-a-c-string
std::string const write_escaped(std::string const& s) {
    std::string out;
    out.reserve(s.size());
    for (std::string::const_iterator i = s.begin(), end = s.end(); i != end; ++i) {
    unsigned char c = *i;
    if (' ' <= c and c <= '~' and c != '\\' and c != '"') {
      out += c;
    }
    else {
      out += '\\';
      switch(c) {
      case '"':  out += '"';  break;
      case '\\': out += '\\'; break;
      case '\t': out += 't';  break;
      case '\r': out += 'r';  break;
      case '\n': out += 'n';  break;
      default:
        char const* const hexdig = "0123456789ABCDEF";
        out += 'x';
        out += hexdig[c >> 4];
        out += hexdig[c & 0xF];
      }
    }
  }
  return out;
}


void write_tree(std::ostream& stream, Provenance* node) {
    // Node label 
    stream << "  " << node->get_id() << " [label=\"" << write_escaped(node->get_full_call()) << "\"];" << std::endl; 
    for(auto parent : node->parents()) {
        //Edge
        stream << "  " << node->get_id() << " -> " << parent->get_id() << ";" << std::endl;
        write_tree(stream, parent);
    }
}

void ProvenanceGraph::toDot(const std::string& filename, int call_id, Provenance* node) {
    std::ofstream file;
    file.open(filename);

    file << "digraph eval_call_" << call_id << " {" << std::endl;

    write_tree(file, node);

    file << "}";
    file.close();
}