#ifndef LEXER_H
#define LEXER_H

#include <string>

enum class TokenType {
    /* Keywords */
    PROGRAM, END, FUNCTION, SUBROUTINE,
    INTEGER, INTENT, IN, INOUT,
    IF, THEN, ELSE, ELSEIF,
    DO, WHILE,
    PRINT, READ,
    CALL, RETURN, STOP,
    EXIT, CYCLE,

    /* Literals and identifiers */
    INT_LIT,
    STR_LIT,
    IDENT,

    /* Operators */
    PLUS, MINUS, STAR, SLASH,
    EQ,         /* == */
    NE,         /* /= */
    LT,         /* < */
    GT,         /* > */
    LE,         /* <= */
    GE,         /* >= */
    AND,        /* .AND. */
    OR,         /* .OR. */
    NOT,        /* .NOT. */
    ASSIGN,     /* = */

    /* Punctuation */
    LPAREN, RPAREN,
    COMMA,
    DCOLON,     /* :: */

    /* Special */
    NEWLINE,
    END_OF_FILE,
    ERROR
};

struct Token {
    TokenType type = TokenType::END_OF_FILE;
    std::string text;
    int int_value = 0;
    int line = 0;
};

class Lexer {
public:
    explicit Lexer(const std::string &source);
    Token next();

    static const char *token_type_name(TokenType type);

private:
    std::string source_;
    int pos_ = 0;
    int line_ = 1;

    char peek() const;
    char advance();
    Token make_token(TokenType type, const std::string &text, int line);
    Token make_int_token(int value, const std::string &text, int line);
    Token make_error(const std::string &msg, int line);
};

#endif
