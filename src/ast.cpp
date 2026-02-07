#include "ast.h"

ASTPtr make_program(const std::string &name, std::vector<ASTPtr> stmts, int line) {
    ProgramNode n;
    n.name = name;
    n.stmts = std::move(stmts);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_func_def(const std::string &name, std::vector<ParamDef> params,
                     std::vector<ASTPtr> body, int line) {
    FuncDefNode n;
    n.name = name;
    n.params = std::move(params);
    n.body = std::move(body);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_sub_def(const std::string &name, std::vector<ParamDef> params,
                    std::vector<ASTPtr> body, int line) {
    SubDefNode n;
    n.name = name;
    n.params = std::move(params);
    n.body = std::move(body);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_assign(const std::string &target, ASTPtr index, ASTPtr value, int line) {
    AssignNode n;
    n.target = target;
    n.index = std::move(index);
    n.value = std::move(value);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_if(ASTPtr cond, std::vector<ASTPtr> then_body, ASTPtr else_branch, int line) {
    IfNode n;
    n.cond = std::move(cond);
    n.then_body = std::move(then_body);
    n.else_branch = std::move(else_branch);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_do_loop(const std::string &var, ASTPtr start, ASTPtr end,
                    ASTPtr step, std::vector<ASTPtr> body, int line) {
    DoLoopNode n;
    n.var = var;
    n.start = std::move(start);
    n.end = std::move(end);
    n.step = std::move(step);
    n.body = std::move(body);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_do_while(ASTPtr cond, std::vector<ASTPtr> body, int line) {
    DoWhileNode n;
    n.cond = std::move(cond);
    n.body = std::move(body);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_print(std::vector<ASTPtr> items, int line) {
    PrintNode n;
    n.items = std::move(items);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_read(std::vector<std::string> vars, std::vector<ASTPtr> indices, int line) {
    ReadNode n;
    n.vars = std::move(vars);
    n.indices = std::move(indices);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_call(const std::string &name, std::vector<ASTPtr> args, int line) {
    CallNode n;
    n.name = name;
    n.args = std::move(args);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_return(int line) {
    return std::make_unique<ASTNode>(line, ReturnNode{});
}

ASTPtr make_stop(int line) {
    return std::make_unique<ASTNode>(line, StopNode{});
}

ASTPtr make_exit(int line) {
    return std::make_unique<ASTNode>(line, ExitNode{});
}

ASTPtr make_cycle(int line) {
    return std::make_unique<ASTNode>(line, CycleNode{});
}

ASTPtr make_decl(std::vector<std::string> names, std::vector<int> sizes,
                 IntentType intent, int line) {
    DeclNode n;
    n.names = std::move(names);
    n.sizes = std::move(sizes);
    n.intent = intent;
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_int_lit(int value, int line) {
    return std::make_unique<ASTNode>(line, IntLitNode{value});
}

ASTPtr make_str_lit(const std::string &value, int line) {
    return std::make_unique<ASTNode>(line, StrLitNode{value});
}

ASTPtr make_var_ref(const std::string &name, int line) {
    return std::make_unique<ASTNode>(line, VarRefNode{name});
}

ASTPtr make_binop(OpType op, ASTPtr left, ASTPtr right, int line) {
    BinOpNode n;
    n.op = op;
    n.left = std::move(left);
    n.right = std::move(right);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_unary(OpType op, ASTPtr operand, int line) {
    UnaryNode n;
    n.op = op;
    n.operand = std::move(operand);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_func_call(const std::string &name, std::vector<ASTPtr> args, int line) {
    FuncCallNode n;
    n.name = name;
    n.args = std::move(args);
    return std::make_unique<ASTNode>(line, std::move(n));
}

ASTPtr make_block(std::vector<ASTPtr> stmts, int line) {
    BlockNode n;
    n.stmts = std::move(stmts);
    return std::make_unique<ASTNode>(line, std::move(n));
}
