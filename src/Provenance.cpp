#include "Provenance.h"


#include <iostream>
#include <fstream>

void ProvenanceGraph::toDot(const std::string& filename) {
    //TODO
}

void write_tree(std::ostream& stream, Provenance* node) {
    // Node label 
    // TODO: escape the label
    stream << "  " << node->get_id() << " [label=\"" << node->get_full_call() << "\"];" << std::endl; 
    for(auto parent : node->parents()) {
        //Edge
        stream << "  " << node->get_id() << " -> " << parent->get_id() << ";" << std::endl;
        write_tree(stream, parent);
    }
}

void ProvenanceGraph::toDot(const std::string& filename, int call_id, Provenance* node) {
    std::ofstream file;
    file.open(filename);

    file << "digraph eval-call-" << call_id << "{" << std::endl;

    write_tree(file, node);

    file << "}";
    file.close();
}