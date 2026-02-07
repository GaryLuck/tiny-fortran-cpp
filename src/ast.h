#ifndef AST_H
#define AST_H

#include <string>
#include <vector>
#include <memory>
#include <variant>

enum class OpType {
    ADD, SUB, MUL, DIV,
    EQ, NE, LT, GT, LE, GE,
    AND, OR, NOT, NEG
};

enum class IntentType {
    NONE,
    IN,
    INOUT
};

struct ParamDef {
    std::string name;
    IntentType intent = IntentType::NONE;
};

struct ASTNode;
using ASTPtr = std::unique_ptr<ASTNode>;

struct ProgramNode {
    std::string name;
    std::vector<ASTPtr> stmts;
};

struct FuncDefNode {
    std::string name;
    std::vector<ParamDef> params;
    std::vector<ASTPtr> body;
};

struct SubDefNode {
    std::string name;
    std::vector<ParamDef> params;
    std::vector<ASTPtr> body;
};

struct AssignNode {
    std::string target;
    ASTPtr index;   /* nullptr for scalar */
    ASTPtr value;
};

struct IfNode {
    ASTPtr cond;
    std::vector<ASTPtr> then_body;
    ASTPtr else_branch;  /* nullptr, another IfNode, or BlockNode */
};

struct DoLoopNode {
    std::string var;
    ASTPtr start;
    ASTPtr end;
    ASTPtr step;    /* nullptr if no step */
    std::vector<ASTPtr> body;
};

struct DoWhileNode {
    ASTPtr cond;
    std::vector<ASTPtr> body;
};

struct PrintNode {
    std::vector<ASTPtr> items;
};

struct ReadNode {
    std::vector<std::string> vars;
    std::vector<ASTPtr> indices;  /* parallel array, nullptr entries for scalars */
};

struct CallNode {
    std::string name;
    std::vector<ASTPtr> args;
};

struct DeclNode {
    std::vector<std::string> names;
    std::vector<int> sizes;   /* 0=scalar, >0=array size */
    IntentType intent = IntentType::NONE;
};

struct IntLitNode {
    int value;
};

struct StrLitNode {
    std::string value;
};

struct VarRefNode {
    std::string name;
};

struct BinOpNode {
    OpType op;
    ASTPtr left;
    ASTPtr right;
};

struct UnaryNode {
    OpType op;
    ASTPtr operand;
};

struct FuncCallNode {
    std::string name;
    std::vector<ASTPtr> args;
};

struct BlockNode {
    std::vector<ASTPtr> stmts;
};

struct ReturnNode {};
struct StopNode {};
struct ExitNode {};
struct CycleNode {};

using NodeData = std::variant<
    ProgramNode,    // 0
    FuncDefNode,    // 1
    SubDefNode,     // 2
    AssignNode,     // 3
    IfNode,         // 4
    DoLoopNode,     // 5
    DoWhileNode,    // 6
    PrintNode,      // 7
    ReadNode,       // 8
    CallNode,       // 9
    DeclNode,       // 10
    IntLitNode,     // 11
    StrLitNode,     // 12
    VarRefNode,     // 13
    BinOpNode,      // 14
    UnaryNode,      // 15
    FuncCallNode,   // 16
    BlockNode,      // 17
    ReturnNode,     // 18
    StopNode,       // 19
    ExitNode,       // 20
    CycleNode       // 21
>;

struct ASTNode {
    int line;
    NodeData data;

    ASTNode(int line, NodeData data) : line(line), data(std::move(data)) {}
};

/* Helper to check node type */
template<typename T>
bool is(const ASTNode &node) { return std::holds_alternative<T>(node.data); }

template<typename T>
bool is(const ASTPtr &node) { return node && std::holds_alternative<T>(node->data); }

template<typename T>
T &as(ASTNode &node) { return std::get<T>(node.data); }

template<typename T>
const T &as(const ASTNode &node) { return std::get<T>(node.data); }

/* Factory functions */
ASTPtr make_program(const std::string &name, std::vector<ASTPtr> stmts, int line);
ASTPtr make_func_def(const std::string &name, std::vector<ParamDef> params,
                     std::vector<ASTPtr> body, int line);
ASTPtr make_sub_def(const std::string &name, std::vector<ParamDef> params,
                    std::vector<ASTPtr> body, int line);
ASTPtr make_assign(const std::string &target, ASTPtr index, ASTPtr value, int line);
ASTPtr make_if(ASTPtr cond, std::vector<ASTPtr> then_body, ASTPtr else_branch, int line);
ASTPtr make_do_loop(const std::string &var, ASTPtr start, ASTPtr end,
                    ASTPtr step, std::vector<ASTPtr> body, int line);
ASTPtr make_do_while(ASTPtr cond, std::vector<ASTPtr> body, int line);
ASTPtr make_print(std::vector<ASTPtr> items, int line);
ASTPtr make_read(std::vector<std::string> vars, std::vector<ASTPtr> indices, int line);
ASTPtr make_call(const std::string &name, std::vector<ASTPtr> args, int line);
ASTPtr make_return(int line);
ASTPtr make_stop(int line);
ASTPtr make_exit(int line);
ASTPtr make_cycle(int line);
ASTPtr make_decl(std::vector<std::string> names, std::vector<int> sizes,
                 IntentType intent, int line);
ASTPtr make_int_lit(int value, int line);
ASTPtr make_str_lit(const std::string &value, int line);
ASTPtr make_var_ref(const std::string &name, int line);
ASTPtr make_binop(OpType op, ASTPtr left, ASTPtr right, int line);
ASTPtr make_unary(OpType op, ASTPtr operand, int line);
ASTPtr make_func_call(const std::string &name, std::vector<ASTPtr> args, int line);
ASTPtr make_block(std::vector<ASTPtr> stmts, int line);

#endif
