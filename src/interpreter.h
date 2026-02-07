#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "ast.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

class Interpreter {
public:
    Interpreter();

    void register_units(const std::vector<ASTPtr> &units);
    int run();

private:
    /* Variable with optional array storage */
    struct Variable {
        int value = 0;
        std::vector<int> array_data;  /* empty for scalars */
        bool is_array() const { return !array_data.empty(); }
    };

    /* Scope for variable lookup */
    struct Scope {
        std::unordered_map<std::string, Variable> vars;
        Scope *parent = nullptr;
    };

    /* Control flow signals */
    enum class Flow { NORMAL, RETURN, STOP, EXIT, CYCLE };

    /* Function/subroutine table */
    std::unordered_map<std::string, ASTNode *> func_table_;
    ASTNode *program_ = nullptr;

    std::unique_ptr<Scope> global_scope_;
    Scope *current_scope_ = nullptr;
    Flow flow_ = Flow::NORMAL;

    /* Scope management */
    Variable *scope_find(const std::string &name);
    void scope_set(const std::string &name, int value);
    void scope_declare(const std::string &name);
    void scope_declare_array(const std::string &name, int size);

    /* Execution */
    void exec_body(const std::vector<ASTPtr> &stmts);
    void exec_stmt(const ASTNode &node);
    int eval_expr(const ASTNode &node);

    /* Intrinsics */
    bool call_intrinsic(const std::string &name, const std::vector<ASTPtr> &args,
                        int line, int &result);

    [[noreturn]] void runtime_error(int line, const std::string &msg);
};

#endif
