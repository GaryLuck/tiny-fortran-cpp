#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include <string>
#include <vector>

struct ParseResult {
    std::vector<ASTPtr> units;
    std::string error;
    int error_line = 0;

    bool has_error() const { return !error.empty(); }
};

ParseResult parse(const std::string &source);

#endif
