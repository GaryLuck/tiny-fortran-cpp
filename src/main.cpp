#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "parser.h"
#include "interpreter.h"

static std::string read_file(const std::string &path) {
    std::ifstream f(path, std::ios::binary);
    if (!f) {
        std::cerr << "Error: Cannot open file '" << path << "'\n";
        return {};
    }
    std::ostringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::cerr << "Tiny Fortran Interpreter (C++)\n";
        std::cerr << "Usage: " << argv[0] << " <source.f90>\n";
        return 1;
    }

    std::string source = read_file(argv[1]);
    if (source.empty()) return 1;

    /* Parse */
    ParseResult pr = parse(source);
    if (pr.has_error()) {
        std::cerr << "Parse error at line " << pr.error_line << ": " << pr.error << "\n";
        return 1;
    }

    if (pr.units.empty()) {
        std::cerr << "Error: No program units found\n";
        return 1;
    }

    /* Interpret */
    Interpreter interp;
    interp.register_units(pr.units);
    return interp.run();
}
