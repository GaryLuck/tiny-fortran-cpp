#include "lexer.h"
#include <cctype>
#include <algorithm>

static std::string str_upper(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c) { return std::toupper(c); });
    return s;
}

struct Keyword {
    const char *word;
    TokenType type;
};

static const Keyword keywords[] = {
    {"PROGRAM",    TokenType::PROGRAM},
    {"END",        TokenType::END},
    {"FUNCTION",   TokenType::FUNCTION},
    {"SUBROUTINE", TokenType::SUBROUTINE},
    {"INTEGER",    TokenType::INTEGER},
    {"INTENT",     TokenType::INTENT},
    {"IN",         TokenType::IN},
    {"INOUT",      TokenType::INOUT},
    {"IF",         TokenType::IF},
    {"THEN",       TokenType::THEN},
    {"ELSE",       TokenType::ELSE},
    {"ELSEIF",     TokenType::ELSEIF},
    {"DO",         TokenType::DO},
    {"WHILE",      TokenType::WHILE},
    {"PRINT",      TokenType::PRINT},
    {"READ",       TokenType::READ},
    {"CALL",       TokenType::CALL},
    {"RETURN",     TokenType::RETURN},
    {"STOP",       TokenType::STOP},
    {"EXIT",       TokenType::EXIT},
    {"CYCLE",      TokenType::CYCLE},
};

static TokenType lookup_keyword(const std::string &upper) {
    for (const auto &kw : keywords) {
        if (upper == kw.word) return kw.type;
    }
    return TokenType::IDENT;
}

Lexer::Lexer(const std::string &source) : source_(source) {}

char Lexer::peek() const {
    if (pos_ >= static_cast<int>(source_.size())) return '\0';
    return source_[pos_];
}

char Lexer::advance() {
    char c = source_[pos_++];
    if (c == '\n') line_++;
    return c;
}

Token Lexer::make_token(TokenType type, const std::string &text, int line) {
    return {type, text, 0, line};
}

Token Lexer::make_int_token(int value, const std::string &text, int line) {
    return {TokenType::INT_LIT, text, value, line};
}

Token Lexer::make_error(const std::string &msg, int line) {
    return {TokenType::ERROR, msg, 0, line};
}

Token Lexer::next() {
    /* Skip spaces and tabs (not newlines) */
    while (pos_ < static_cast<int>(source_.size())) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else {
            break;
        }
    }

    if (pos_ >= static_cast<int>(source_.size())) {
        return make_token(TokenType::END_OF_FILE, "", line_);
    }

    int line = line_;
    char c = peek();

    /* Newline */
    if (c == '\n') {
        advance();
        return make_token(TokenType::NEWLINE, "\\n", line);
    }

    /* Comment: skip to end of line */
    if (c == '!') {
        while (pos_ < static_cast<int>(source_.size()) && peek() != '\n') {
            advance();
        }
        return next();
    }

    /* String literal */
    if (c == '"' || c == '\'') {
        char quote = c;
        advance();
        int start = pos_;
        while (pos_ < static_cast<int>(source_.size()) && peek() != quote && peek() != '\n') {
            advance();
        }
        std::string text = source_.substr(start, pos_ - start);
        if (pos_ < static_cast<int>(source_.size()) && peek() == quote) {
            advance();
        }
        return {TokenType::STR_LIT, text, 0, line};
    }

    /* Integer literal */
    if (std::isdigit(static_cast<unsigned char>(c))) {
        int start = pos_;
        while (pos_ < static_cast<int>(source_.size()) && std::isdigit(static_cast<unsigned char>(peek()))) {
            advance();
        }
        std::string text = source_.substr(start, pos_ - start);
        int val = std::stoi(text);
        return make_int_token(val, text, line);
    }

    /* Identifier or keyword */
    if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
        int start = pos_;
        while (pos_ < static_cast<int>(source_.size()) &&
               (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_')) {
            advance();
        }
        std::string text = source_.substr(start, pos_ - start);
        std::string upper = str_upper(text);
        TokenType type = lookup_keyword(upper);
        return make_token(type, upper, line);
    }

    /* Dot-operators: .AND. .OR. .NOT. */
    if (c == '.') {
        int start = pos_;
        advance();
        if (pos_ < static_cast<int>(source_.size()) && std::isalpha(static_cast<unsigned char>(peek()))) {
            int wstart = pos_;
            while (pos_ < static_cast<int>(source_.size()) && std::isalpha(static_cast<unsigned char>(peek()))) {
                advance();
            }
            if (pos_ < static_cast<int>(source_.size()) && peek() == '.') {
                advance();
                std::string word = str_upper(source_.substr(wstart, pos_ - wstart - 1));
                if (word == "AND") return make_token(TokenType::AND, ".AND.", line);
                if (word == "OR")  return make_token(TokenType::OR, ".OR.", line);
                if (word == "NOT") return make_token(TokenType::NOT, ".NOT.", line);
                if (word == "TRUE")  return make_int_token(1, ".TRUE.", line);
                if (word == "FALSE") return make_int_token(0, ".FALSE.", line);
            }
        }
        pos_ = start + 1;
        return make_error("Unexpected '.'", line);
    }

    /* Two-character operators */
    advance();
    if (pos_ < static_cast<int>(source_.size())) {
        char c2 = peek();

        if (c == ':' && c2 == ':') { advance(); return make_token(TokenType::DCOLON, "::", line); }
        if (c == '=' && c2 == '=') { advance(); return make_token(TokenType::EQ, "==", line); }
        if (c == '/' && c2 == '=') { advance(); return make_token(TokenType::NE, "/=", line); }
        if (c == '<' && c2 == '=') { advance(); return make_token(TokenType::LE, "<=", line); }
        if (c == '>' && c2 == '=') { advance(); return make_token(TokenType::GE, ">=", line); }
    }

    /* Single-character operators */
    switch (c) {
        case '+': return make_token(TokenType::PLUS, "+", line);
        case '-': return make_token(TokenType::MINUS, "-", line);
        case '*': return make_token(TokenType::STAR, "*", line);
        case '/': return make_token(TokenType::SLASH, "/", line);
        case '<': return make_token(TokenType::LT, "<", line);
        case '>': return make_token(TokenType::GT, ">", line);
        case '=': return make_token(TokenType::ASSIGN, "=", line);
        case '(': return make_token(TokenType::LPAREN, "(", line);
        case ')': return make_token(TokenType::RPAREN, ")", line);
        case ',': return make_token(TokenType::COMMA, ",", line);
    }

    /* Semicolons act as statement separators like newlines */
    if (c == ';') {
        return make_token(TokenType::NEWLINE, ";", line);
    }

    return make_error(std::string("Unexpected character '") + c + "'", line);
}

const char *Lexer::token_type_name(TokenType type) {
    switch (type) {
        case TokenType::PROGRAM:     return "PROGRAM";
        case TokenType::END:         return "END";
        case TokenType::FUNCTION:    return "FUNCTION";
        case TokenType::SUBROUTINE:  return "SUBROUTINE";
        case TokenType::INTEGER:     return "INTEGER";
        case TokenType::INTENT:      return "INTENT";
        case TokenType::IN:          return "IN";
        case TokenType::INOUT:       return "INOUT";
        case TokenType::IF:          return "IF";
        case TokenType::THEN:        return "THEN";
        case TokenType::ELSE:        return "ELSE";
        case TokenType::ELSEIF:      return "ELSEIF";
        case TokenType::DO:          return "DO";
        case TokenType::WHILE:       return "WHILE";
        case TokenType::PRINT:       return "PRINT";
        case TokenType::READ:        return "READ";
        case TokenType::CALL:        return "CALL";
        case TokenType::RETURN:      return "RETURN";
        case TokenType::STOP:        return "STOP";
        case TokenType::EXIT:        return "EXIT";
        case TokenType::CYCLE:       return "CYCLE";
        case TokenType::INT_LIT:     return "INT_LIT";
        case TokenType::STR_LIT:     return "STR_LIT";
        case TokenType::IDENT:       return "IDENT";
        case TokenType::PLUS:        return "PLUS";
        case TokenType::MINUS:       return "MINUS";
        case TokenType::STAR:        return "STAR";
        case TokenType::SLASH:       return "SLASH";
        case TokenType::EQ:          return "EQ";
        case TokenType::NE:          return "NE";
        case TokenType::LT:          return "LT";
        case TokenType::GT:          return "GT";
        case TokenType::LE:          return "LE";
        case TokenType::GE:          return "GE";
        case TokenType::AND:         return "AND";
        case TokenType::OR:          return "OR";
        case TokenType::NOT:         return "NOT";
        case TokenType::ASSIGN:      return "ASSIGN";
        case TokenType::LPAREN:      return "LPAREN";
        case TokenType::RPAREN:      return "RPAREN";
        case TokenType::COMMA:       return "COMMA";
        case TokenType::DCOLON:      return "DCOLON";
        case TokenType::NEWLINE:     return "NEWLINE";
        case TokenType::END_OF_FILE: return "EOF";
        case TokenType::ERROR:       return "ERROR";
    }
    return "UNKNOWN";
}
