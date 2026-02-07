#include "parser.h"
#include "lexer.h"
#include <algorithm>
#include <cctype>

static std::string str_upper(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c) { return std::toupper(c); });
    return s;
}

/* ── Parser state ─────────────────────────────────────────────── */

class Parser {
public:
    explicit Parser(const std::string &source);

    ParseResult parse_all();

private:
    Lexer lex_;
    Token cur_;
    bool has_error_ = false;
    std::string error_msg_;
    int error_line_ = 0;

    void error(const std::string &msg);
    void next();
    bool check(TokenType t) const;
    bool match(TokenType t);
    void expect(TokenType t);
    void skip_newlines();

    /* Expression parsing */
    ASTPtr parse_expr();
    ASTPtr parse_expr_prec(int min_prec);
    ASTPtr parse_unary();
    ASTPtr parse_primary();

    /* Statement parsing */
    ASTPtr parse_statement();
    std::vector<ASTPtr> parse_body();
    bool at_end_block() const;

    ASTPtr parse_if_stmt();
    ASTPtr parse_do_stmt();
    ASTPtr parse_print_stmt();
    ASTPtr parse_read_stmt();
    ASTPtr parse_call_stmt();
    ASTPtr parse_decl();

    /* Top-level */
    std::vector<ParamDef> parse_params();
    void update_param_intents(std::vector<ParamDef> &params,
                              const std::vector<ASTPtr> &body);
    ASTPtr parse_program();
    ASTPtr parse_function();
    ASTPtr parse_subroutine();
};

/* ── Implementation ───────────────────────────────────────────── */

Parser::Parser(const std::string &source) : lex_(source) {
    cur_ = lex_.next();
}

void Parser::error(const std::string &msg) {
    if (!has_error_) {
        has_error_ = true;
        error_msg_ = msg;
        error_line_ = cur_.line;
    }
}

void Parser::next() {
    cur_ = lex_.next();
}

bool Parser::check(TokenType t) const {
    return cur_.type == t;
}

bool Parser::match(TokenType t) {
    if (cur_.type == t) {
        next();
        return true;
    }
    return false;
}

void Parser::expect(TokenType t) {
    if (cur_.type == t) {
        next();
    } else {
        error(std::string("Expected ") + Lexer::token_type_name(t) +
              ", got " + Lexer::token_type_name(cur_.type) +
              " '" + cur_.text + "'");
    }
}

void Parser::skip_newlines() {
    while (cur_.type == TokenType::NEWLINE) {
        next();
    }
}

/* ── Expression parsing (precedence climbing) ─────────────────── */

static int get_prec(TokenType t) {
    switch (t) {
        case TokenType::OR:    return 1;
        case TokenType::AND:   return 2;
        case TokenType::EQ: case TokenType::NE:
        case TokenType::LT: case TokenType::GT:
        case TokenType::LE: case TokenType::GE:
            return 3;
        case TokenType::PLUS: case TokenType::MINUS:
            return 4;
        case TokenType::STAR: case TokenType::SLASH:
            return 5;
        default:
            return -1;
    }
}

static OpType tok_to_op(TokenType t) {
    switch (t) {
        case TokenType::PLUS:  return OpType::ADD;
        case TokenType::MINUS: return OpType::SUB;
        case TokenType::STAR:  return OpType::MUL;
        case TokenType::SLASH: return OpType::DIV;
        case TokenType::EQ:    return OpType::EQ;
        case TokenType::NE:    return OpType::NE;
        case TokenType::LT:    return OpType::LT;
        case TokenType::GT:    return OpType::GT;
        case TokenType::LE:    return OpType::LE;
        case TokenType::GE:    return OpType::GE;
        case TokenType::AND:   return OpType::AND;
        case TokenType::OR:    return OpType::OR;
        default:               return OpType::ADD;
    }
}

ASTPtr Parser::parse_primary() {
    int line = cur_.line;
    if (has_error_) return make_int_lit(0, line);

    if (check(TokenType::INT_LIT)) {
        int val = cur_.int_value;
        next();
        return make_int_lit(val, line);
    }

    if (check(TokenType::STR_LIT)) {
        std::string val = cur_.text;
        next();
        return make_str_lit(val, line);
    }

    if (check(TokenType::LPAREN)) {
        next();
        auto e = parse_expr();
        expect(TokenType::RPAREN);
        return e;
    }

    if (check(TokenType::IDENT)) {
        std::string name = str_upper(cur_.text);
        next();

        if (check(TokenType::LPAREN)) {
            next();
            std::vector<ASTPtr> args;
            if (!check(TokenType::RPAREN)) {
                args.push_back(parse_expr());
                while (match(TokenType::COMMA)) {
                    args.push_back(parse_expr());
                }
            }
            expect(TokenType::RPAREN);
            return make_func_call(name, std::move(args), line);
        }

        return make_var_ref(name, line);
    }

    error(std::string("Expected expression, got ") +
          Lexer::token_type_name(cur_.type) + " '" + cur_.text + "'");
    return make_int_lit(0, line);
}

ASTPtr Parser::parse_unary() {
    int line = cur_.line;

    if (check(TokenType::MINUS)) {
        next();
        auto operand = parse_unary();
        return make_unary(OpType::NEG, std::move(operand), line);
    }
    if (check(TokenType::NOT)) {
        next();
        auto operand = parse_unary();
        return make_unary(OpType::NOT, std::move(operand), line);
    }
    if (check(TokenType::PLUS)) {
        next();
        return parse_unary();
    }
    return parse_primary();
}

ASTPtr Parser::parse_expr_prec(int min_prec) {
    auto left = parse_unary();

    while (!has_error_) {
        int prec = get_prec(cur_.type);
        if (prec < min_prec) break;

        TokenType op_tok = cur_.type;
        int line = cur_.line;
        next();

        auto right = parse_expr_prec(prec + 1);
        left = make_binop(tok_to_op(op_tok), std::move(left), std::move(right), line);
    }
    return left;
}

ASTPtr Parser::parse_expr() {
    return parse_expr_prec(1);
}

/* ── Statement parsing ────────────────────────────────────────── */

bool Parser::at_end_block() const {
    return check(TokenType::END) || check(TokenType::ELSE) ||
           check(TokenType::ELSEIF) || check(TokenType::END_OF_FILE);
}

std::vector<ASTPtr> Parser::parse_body() {
    std::vector<ASTPtr> stmts;
    skip_newlines();

    while (!has_error_ && !at_end_block()) {
        if (check(TokenType::ELSE)) break;

        auto s = parse_statement();
        if (s) stmts.push_back(std::move(s));
        skip_newlines();
    }
    return stmts;
}

ASTPtr Parser::parse_if_stmt() {
    int line = cur_.line;
    expect(TokenType::LPAREN);
    auto cond = parse_expr();
    expect(TokenType::RPAREN);
    expect(TokenType::THEN);
    skip_newlines();

    auto then_body = parse_body();
    ASTPtr else_branch;

    if (check(TokenType::ELSEIF)) {
        next();
        else_branch = parse_if_stmt();
        return make_if(std::move(cond), std::move(then_body), std::move(else_branch), line);
    }

    if (check(TokenType::ELSE)) {
        next();
        skip_newlines();
        if (check(TokenType::IF)) {
            next();
            else_branch = parse_if_stmt();
            return make_if(std::move(cond), std::move(then_body), std::move(else_branch), line);
        }
        auto else_body = parse_body();
        else_branch = make_block(std::move(else_body), cur_.line);
    }

    expect(TokenType::END);
    if (check(TokenType::IF)) next();

    return make_if(std::move(cond), std::move(then_body), std::move(else_branch), line);
}

ASTPtr Parser::parse_do_stmt() {
    int line = cur_.line;

    if (check(TokenType::WHILE)) {
        next();
        expect(TokenType::LPAREN);
        auto cond = parse_expr();
        expect(TokenType::RPAREN);
        skip_newlines();
        auto body = parse_body();
        expect(TokenType::END);
        if (check(TokenType::DO)) next();
        return make_do_while(std::move(cond), std::move(body), line);
    }

    if (!check(TokenType::IDENT)) {
        error("Expected loop variable after DO");
        return make_int_lit(0, line);
    }

    std::string var = str_upper(cur_.text);
    next();
    expect(TokenType::ASSIGN);
    auto start = parse_expr();
    expect(TokenType::COMMA);
    auto end = parse_expr();

    ASTPtr step;
    if (match(TokenType::COMMA)) {
        step = parse_expr();
    }
    skip_newlines();

    auto body = parse_body();
    expect(TokenType::END);
    if (check(TokenType::DO)) next();

    return make_do_loop(var, std::move(start), std::move(end),
                        std::move(step), std::move(body), line);
}

ASTPtr Parser::parse_print_stmt() {
    int line = cur_.line;
    expect(TokenType::STAR);
    match(TokenType::COMMA);

    std::vector<ASTPtr> items;
    if (!check(TokenType::NEWLINE) && !check(TokenType::END_OF_FILE)) {
        items.push_back(parse_expr());
        while (match(TokenType::COMMA)) {
            items.push_back(parse_expr());
        }
    }
    return make_print(std::move(items), line);
}

ASTPtr Parser::parse_read_stmt() {
    int line = cur_.line;
    expect(TokenType::STAR);
    match(TokenType::COMMA);

    std::vector<std::string> vars;
    std::vector<ASTPtr> indices;

    if (check(TokenType::IDENT)) {
        vars.push_back(str_upper(cur_.text));
        next();
        if (check(TokenType::LPAREN)) {
            next();
            indices.push_back(parse_expr());
            expect(TokenType::RPAREN);
        } else {
            indices.push_back(nullptr);
        }
        while (match(TokenType::COMMA)) {
            if (check(TokenType::IDENT)) {
                vars.push_back(str_upper(cur_.text));
                next();
                if (check(TokenType::LPAREN)) {
                    next();
                    indices.push_back(parse_expr());
                    expect(TokenType::RPAREN);
                } else {
                    indices.push_back(nullptr);
                }
            }
        }
    }
    return make_read(std::move(vars), std::move(indices), line);
}

ASTPtr Parser::parse_call_stmt() {
    int line = cur_.line;
    if (!check(TokenType::IDENT)) {
        error("Expected subroutine name after CALL");
        return make_int_lit(0, line);
    }
    std::string name = str_upper(cur_.text);
    next();

    std::vector<ASTPtr> args;
    if (match(TokenType::LPAREN)) {
        if (!check(TokenType::RPAREN)) {
            args.push_back(parse_expr());
            while (match(TokenType::COMMA)) {
                args.push_back(parse_expr());
            }
        }
        expect(TokenType::RPAREN);
    }
    return make_call(name, std::move(args), line);
}

ASTPtr Parser::parse_decl() {
    int line = cur_.line;
    IntentType intent = IntentType::NONE;

    if (check(TokenType::COMMA)) {
        next();
        if (check(TokenType::INTENT)) {
            next();
            expect(TokenType::LPAREN);
            if (check(TokenType::INOUT)) {
                intent = IntentType::INOUT;
                next();
            } else if (check(TokenType::IN)) {
                intent = IntentType::IN;
                next();
            } else {
                error("Expected IN or INOUT");
            }
            expect(TokenType::RPAREN);
        } else {
            error("Expected INTENT after comma in declaration");
        }
    }

    expect(TokenType::DCOLON);

    std::vector<std::string> names;
    std::vector<int> sizes;

    if (check(TokenType::IDENT)) {
        names.push_back(str_upper(cur_.text));
        next();
        int sz = 0;
        if (check(TokenType::LPAREN)) {
            next();
            if (check(TokenType::INT_LIT)) {
                sz = cur_.int_value;
                next();
            } else {
                error("Array size must be an integer literal");
            }
            expect(TokenType::RPAREN);
        }
        sizes.push_back(sz);

        while (match(TokenType::COMMA)) {
            if (check(TokenType::IDENT)) {
                names.push_back(str_upper(cur_.text));
                next();
                sz = 0;
                if (check(TokenType::LPAREN)) {
                    next();
                    if (check(TokenType::INT_LIT)) {
                        sz = cur_.int_value;
                        next();
                    } else {
                        error("Array size must be an integer literal");
                    }
                    expect(TokenType::RPAREN);
                }
                sizes.push_back(sz);
            }
        }
    }
    return make_decl(std::move(names), std::move(sizes), intent, line);
}

ASTPtr Parser::parse_statement() {
    skip_newlines();
    if (has_error_ || check(TokenType::END_OF_FILE)) return nullptr;

    int line = cur_.line;

    if (check(TokenType::IF))      { next(); return parse_if_stmt(); }
    if (check(TokenType::DO))      { next(); return parse_do_stmt(); }
    if (check(TokenType::PRINT))   { next(); return parse_print_stmt(); }
    if (check(TokenType::READ))    { next(); return parse_read_stmt(); }
    if (check(TokenType::CALL))    { next(); return parse_call_stmt(); }
    if (check(TokenType::RETURN))  { next(); return make_return(line); }
    if (check(TokenType::STOP))    { next(); return make_stop(line); }
    if (check(TokenType::EXIT))    { next(); return make_exit(line); }
    if (check(TokenType::CYCLE))   { next(); return make_cycle(line); }
    if (check(TokenType::INTEGER)) { next(); return parse_decl(); }

    if (check(TokenType::IDENT)) {
        std::string name = str_upper(cur_.text);
        next();

        if (check(TokenType::LPAREN)) {
            next();
            auto index = parse_expr();
            expect(TokenType::RPAREN);
            if (match(TokenType::ASSIGN)) {
                auto value = parse_expr();
                return make_assign(name, std::move(index), std::move(value), line);
            }
            error("Expected '=' after '" + name + "(...)' for array assignment");
            return nullptr;
        }

        if (match(TokenType::ASSIGN)) {
            auto value = parse_expr();
            return make_assign(name, nullptr, std::move(value), line);
        }

        error("Expected '=' after '" + name + "' for assignment");
        return nullptr;
    }

    error(std::string("Unexpected token ") + Lexer::token_type_name(cur_.type) +
          " '" + cur_.text + "'");
    next();
    return nullptr;
}

/* ── Top-level parsing ────────────────────────────────────────── */

std::vector<ParamDef> Parser::parse_params() {
    std::vector<ParamDef> params;

    if (match(TokenType::LPAREN)) {
        if (!check(TokenType::RPAREN)) {
            if (check(TokenType::IDENT)) {
                params.push_back({str_upper(cur_.text), IntentType::NONE});
                next();
            }
            while (match(TokenType::COMMA)) {
                if (check(TokenType::IDENT)) {
                    params.push_back({str_upper(cur_.text), IntentType::NONE});
                    next();
                }
            }
        }
        expect(TokenType::RPAREN);
    }
    return params;
}

void Parser::update_param_intents(std::vector<ParamDef> &params,
                                  const std::vector<ASTPtr> &body) {
    for (const auto &stmt : body) {
        if (!stmt || !is<DeclNode>(*stmt)) continue;
        const auto &decl = as<DeclNode>(*stmt);
        if (decl.intent == IntentType::NONE) continue;
        for (const auto &dname : decl.names) {
            for (auto &p : params) {
                if (p.name == dname) {
                    p.intent = decl.intent;
                }
            }
        }
    }
}

ASTPtr Parser::parse_program() {
    int line = cur_.line;
    expect(TokenType::PROGRAM);
    std::string name = str_upper(cur_.text);
    next();
    skip_newlines();

    auto stmts = parse_body();

    expect(TokenType::END);
    if (check(TokenType::PROGRAM)) {
        next();
        if (check(TokenType::IDENT)) next();
    }

    return make_program(name, std::move(stmts), line);
}

ASTPtr Parser::parse_function() {
    int line = cur_.line;

    if (!check(TokenType::IDENT)) {
        error("Expected function name");
        return make_int_lit(0, line);
    }
    std::string name = str_upper(cur_.text);
    next();

    auto params = parse_params();
    skip_newlines();
    auto body = parse_body();
    update_param_intents(params, body);

    expect(TokenType::END);
    if (check(TokenType::FUNCTION)) {
        next();
        if (check(TokenType::IDENT)) next();
    }

    return make_func_def(name, std::move(params), std::move(body), line);
}

ASTPtr Parser::parse_subroutine() {
    int line = cur_.line;

    if (!check(TokenType::IDENT)) {
        error("Expected subroutine name");
        return make_int_lit(0, line);
    }
    std::string name = str_upper(cur_.text);
    next();

    auto params = parse_params();
    skip_newlines();
    auto body = parse_body();
    update_param_intents(params, body);

    expect(TokenType::END);
    if (check(TokenType::SUBROUTINE)) {
        next();
        if (check(TokenType::IDENT)) next();
    }

    return make_sub_def(name, std::move(params), std::move(body), line);
}

ParseResult Parser::parse_all() {
    std::vector<ASTPtr> units;
    skip_newlines();

    while (!has_error_ && cur_.type != TokenType::END_OF_FILE) {
        if (check(TokenType::PROGRAM)) {
            units.push_back(parse_program());
        } else if (check(TokenType::INTEGER)) {
            next();
            if (check(TokenType::FUNCTION)) {
                next();
                units.push_back(parse_function());
            } else {
                error("Expected FUNCTION after INTEGER at top level");
            }
        } else if (check(TokenType::FUNCTION)) {
            next();
            units.push_back(parse_function());
        } else if (check(TokenType::SUBROUTINE)) {
            next();
            units.push_back(parse_subroutine());
        } else {
            error(std::string("Expected PROGRAM, FUNCTION, or SUBROUTINE, got ") +
                  Lexer::token_type_name(cur_.type));
        }
        skip_newlines();
    }

    ParseResult result;
    result.units = std::move(units);
    if (has_error_) {
        result.error = error_msg_;
        result.error_line = error_line_;
    }
    return result;
}

/* ── Public API ───────────────────────────────────────────────── */

ParseResult parse(const std::string &source) {
    Parser p(source);
    return p.parse_all();
}
