#include "interpreter.h"
#include <iostream>
#include <stdexcept>
#include <cstdlib>

/* ── Exception for STOP statement ─────────────────────────────── */

struct StopException {};

/* ── Interpreter implementation ───────────────────────────────── */

Interpreter::Interpreter() {
    global_scope_ = std::make_unique<Scope>();
    current_scope_ = global_scope_.get();
}

void Interpreter::runtime_error(int line, const std::string &msg) {
    std::cerr << "Runtime error at line " << line << ": " << msg << "\n";
    std::exit(1);
}

/* ── Scope operations ─────────────────────────────────────────── */

Interpreter::Variable *Interpreter::scope_find(const std::string &name) {
    for (Scope *s = current_scope_; s; s = s->parent) {
        auto it = s->vars.find(name);
        if (it != s->vars.end()) return &it->second;
    }
    return nullptr;
}

void Interpreter::scope_set(const std::string &name, int value) {
    auto it = current_scope_->vars.find(name);
    if (it != current_scope_->vars.end()) {
        it->second.value = value;
        return;
    }
    current_scope_->vars[name].value = value;
}

void Interpreter::scope_declare(const std::string &name) {
    if (current_scope_->vars.find(name) == current_scope_->vars.end()) {
        current_scope_->vars[name] = Variable{};
    }
}

void Interpreter::scope_declare_array(const std::string &name, int size) {
    if (current_scope_->vars.find(name) != current_scope_->vars.end()) return;
    Variable v;
    v.array_data.resize(size, 0);
    current_scope_->vars[name] = std::move(v);
}

/* ── Registration ─────────────────────────────────────────────── */

void Interpreter::register_units(const std::vector<ASTPtr> &units) {
    for (const auto &u : units) {
        if (is<ProgramNode>(*u)) {
            program_ = u.get();
        } else if (is<FuncDefNode>(*u)) {
            func_table_[as<FuncDefNode>(*u).name] = u.get();
        } else if (is<SubDefNode>(*u)) {
            func_table_[as<SubDefNode>(*u).name] = u.get();
        }
    }
}

/* ── Intrinsic functions ──────────────────────────────────────── */

bool Interpreter::call_intrinsic(const std::string &name, const std::vector<ASTPtr> &args,
                                 int line, int &result) {
    if (name == "MOD") {
        if (args.size() != 2)
            runtime_error(line, "MOD requires 2 arguments, got " + std::to_string(args.size()));
        int a = eval_expr(*args[0]);
        int b = eval_expr(*args[1]);
        if (b == 0) runtime_error(line, "Division by zero in MOD");
        result = a % b;
        return true;
    }
    if (name == "ABS") {
        if (args.size() != 1)
            runtime_error(line, "ABS requires 1 argument, got " + std::to_string(args.size()));
        int a = eval_expr(*args[0]);
        result = a < 0 ? -a : a;
        return true;
    }
    return false;
}

/* ── Expression evaluation ────────────────────────────────────── */

int Interpreter::eval_expr(const ASTNode &node) {
    if (is<IntLitNode>(node)) {
        return as<IntLitNode>(node).value;
    }

    if (is<StrLitNode>(node)) {
        runtime_error(node.line, "String used in integer expression");
    }

    if (is<VarRefNode>(node)) {
        const auto &vr = as<VarRefNode>(node);
        Variable *v = scope_find(vr.name);
        if (!v) runtime_error(node.line, "Undefined variable '" + vr.name + "'");
        return v->value;
    }

    if (is<BinOpNode>(node)) {
        const auto &bn = as<BinOpNode>(node);
        int left = eval_expr(*bn.left);
        int right = eval_expr(*bn.right);
        switch (bn.op) {
            case OpType::ADD: return left + right;
            case OpType::SUB: return left - right;
            case OpType::MUL: return left * right;
            case OpType::DIV:
                if (right == 0) runtime_error(node.line, "Division by zero");
                return left / right;
            case OpType::EQ:  return left == right ? 1 : 0;
            case OpType::NE:  return left != right ? 1 : 0;
            case OpType::LT:  return left < right  ? 1 : 0;
            case OpType::GT:  return left > right  ? 1 : 0;
            case OpType::LE:  return left <= right ? 1 : 0;
            case OpType::GE:  return left >= right ? 1 : 0;
            case OpType::AND: return (left && right) ? 1 : 0;
            case OpType::OR:  return (left || right) ? 1 : 0;
            default:
                runtime_error(node.line, "Unknown binary operator");
        }
    }

    if (is<UnaryNode>(node)) {
        const auto &un = as<UnaryNode>(node);
        int operand = eval_expr(*un.operand);
        switch (un.op) {
            case OpType::NEG: return -operand;
            case OpType::NOT: return operand ? 0 : 1;
            default:
                runtime_error(node.line, "Unknown unary operator");
        }
    }

    if (is<FuncCallNode>(node)) {
        const auto &fc = as<FuncCallNode>(node);

        /* Check if this is an array element access */
        Variable *av = scope_find(fc.name);
        if (av && av->is_array()) {
            if (fc.args.size() != 1)
                runtime_error(node.line, "Array '" + fc.name + "' requires exactly 1 index");
            int idx = eval_expr(*fc.args[0]);
            if (idx < 1 || idx > static_cast<int>(av->array_data.size()))
                runtime_error(node.line, "Array index " + std::to_string(idx) +
                              " out of bounds for '" + fc.name + "' (1.." +
                              std::to_string(av->array_data.size()) + ")");
            return av->array_data[idx - 1];
        }

        /* Try intrinsics */
        int result;
        if (call_intrinsic(fc.name, fc.args, node.line, result)) {
            return result;
        }

        /* Look up user function */
        auto it = func_table_.find(fc.name);
        if (it == func_table_.end() || !is<FuncDefNode>(*it->second))
            runtime_error(node.line, "Undefined function '" + fc.name + "'");

        const auto &fdef = as<FuncDefNode>(*it->second);
        if (fc.args.size() != fdef.params.size())
            runtime_error(node.line, "Function '" + fc.name + "' expects " +
                          std::to_string(fdef.params.size()) + " arguments, got " +
                          std::to_string(fc.args.size()));

        /* Evaluate arguments */
        std::vector<int> arg_vals;
        for (const auto &arg : fc.args) {
            arg_vals.push_back(eval_expr(*arg));
        }

        /* Push new scope */
        Scope func_scope;
        func_scope.parent = global_scope_.get();
        Scope *caller_scope = current_scope_;
        current_scope_ = &func_scope;

        /* Bind parameters */
        for (size_t i = 0; i < fdef.params.size(); i++) {
            scope_set(fdef.params[i].name, arg_vals[i]);
        }

        /* Initialize function name variable (for return value) */
        scope_declare(fc.name);

        /* Execute body */
        Flow saved_flow = flow_;
        flow_ = Flow::NORMAL;
        exec_body(fdef.body);
        flow_ = saved_flow;

        /* Get return value */
        auto rv_it = func_scope.vars.find(fc.name);
        result = (rv_it != func_scope.vars.end()) ? rv_it->second.value : 0;

        current_scope_ = caller_scope;
        return result;
    }

    runtime_error(node.line, "Invalid expression node");
    return 0; /* unreachable */
}

/* ── Statement execution ──────────────────────────────────────── */

void Interpreter::exec_body(const std::vector<ASTPtr> &stmts) {
    for (const auto &stmt : stmts) {
        if (flow_ != Flow::NORMAL) break;
        exec_stmt(*stmt);
    }
}

void Interpreter::exec_stmt(const ASTNode &node) {
    if (flow_ != Flow::NORMAL) return;

    if (is<DeclNode>(node)) {
        const auto &decl = as<DeclNode>(node);
        for (size_t i = 0; i < decl.names.size(); i++) {
            if (decl.sizes[i] > 0) {
                scope_declare_array(decl.names[i], decl.sizes[i]);
            } else {
                scope_declare(decl.names[i]);
            }
        }
        return;
    }

    if (is<AssignNode>(node)) {
        const auto &assign = as<AssignNode>(node);
        int value = eval_expr(*assign.value);
        if (assign.index) {
            int idx = eval_expr(*assign.index);
            Variable *v = scope_find(assign.target);
            if (!v || !v->is_array())
                runtime_error(node.line, "'" + assign.target + "' is not an array");
            if (idx < 1 || idx > static_cast<int>(v->array_data.size()))
                runtime_error(node.line, "Array index " + std::to_string(idx) +
                              " out of bounds for '" + assign.target + "'");
            v->array_data[idx - 1] = value;
        } else {
            Variable *v = scope_find(assign.target);
            if (v) {
                v->value = value;
            } else {
                scope_set(assign.target, value);
            }
        }
        return;
    }

    if (is<IfNode>(node)) {
        const auto &if_stmt = as<IfNode>(node);
        int cond = eval_expr(*if_stmt.cond);
        if (cond) {
            exec_body(if_stmt.then_body);
        } else if (if_stmt.else_branch) {
            if (is<IfNode>(*if_stmt.else_branch)) {
                exec_stmt(*if_stmt.else_branch);
            } else if (is<BlockNode>(*if_stmt.else_branch)) {
                exec_body(as<BlockNode>(*if_stmt.else_branch).stmts);
            }
        }
        return;
    }

    if (is<DoLoopNode>(node)) {
        const auto &dl = as<DoLoopNode>(node);
        int start = eval_expr(*dl.start);
        int end = eval_expr(*dl.end);
        int step = dl.step ? eval_expr(*dl.step) : 1;

        if (step == 0) runtime_error(node.line, "DO loop step cannot be zero");

        Variable *v = scope_find(dl.var);
        if (!v) {
            scope_set(dl.var, start);
            v = scope_find(dl.var);
        } else {
            v->value = start;
        }

        while ((step > 0 && v->value <= end) || (step < 0 && v->value >= end)) {
            exec_body(dl.body);
            if (flow_ == Flow::EXIT)  { flow_ = Flow::NORMAL; break; }
            if (flow_ == Flow::CYCLE) { flow_ = Flow::NORMAL; }
            if (flow_ != Flow::NORMAL) break;
            v->value += step;
        }
        return;
    }

    if (is<DoWhileNode>(node)) {
        const auto &dw = as<DoWhileNode>(node);
        while (flow_ == Flow::NORMAL) {
            int cond = eval_expr(*dw.cond);
            if (!cond) break;
            exec_body(dw.body);
            if (flow_ == Flow::EXIT)  { flow_ = Flow::NORMAL; break; }
            if (flow_ == Flow::CYCLE) { flow_ = Flow::NORMAL; }
        }
        return;
    }

    if (is<PrintNode>(node)) {
        const auto &pr = as<PrintNode>(node);
        for (size_t i = 0; i < pr.items.size(); i++) {
            const auto &item = *pr.items[i];
            if (is<StrLitNode>(item)) {
                std::cout << as<StrLitNode>(item).value;
            } else {
                int val = eval_expr(item);
                if (i > 0) std::cout << " ";
                std::cout << val;
            }
        }
        std::cout << "\n";
        return;
    }

    if (is<ReadNode>(node)) {
        const auto &rd = as<ReadNode>(node);
        for (size_t i = 0; i < rd.vars.size(); i++) {
            int val;
            if (!(std::cin >> val)) {
                runtime_error(node.line, "Failed to read integer from input");
            }
            if (i < rd.indices.size() && rd.indices[i]) {
                int idx = eval_expr(*rd.indices[i]);
                Variable *v = scope_find(rd.vars[i]);
                if (!v || !v->is_array())
                    runtime_error(node.line, "'" + rd.vars[i] + "' is not an array");
                if (idx < 1 || idx > static_cast<int>(v->array_data.size()))
                    runtime_error(node.line, "Array index out of bounds for '" + rd.vars[i] + "'");
                v->array_data[idx - 1] = val;
            } else {
                Variable *v = scope_find(rd.vars[i]);
                if (v) {
                    v->value = val;
                } else {
                    scope_set(rd.vars[i], val);
                }
            }
        }
        return;
    }

    if (is<CallNode>(node)) {
        const auto &call = as<CallNode>(node);
        auto it = func_table_.find(call.name);
        if (it == func_table_.end() || !is<SubDefNode>(*it->second))
            runtime_error(node.line, "Undefined subroutine '" + call.name + "'");

        const auto &sdef = as<SubDefNode>(*it->second);
        if (call.args.size() != sdef.params.size())
            runtime_error(node.line, "Subroutine '" + call.name + "' expects " +
                          std::to_string(sdef.params.size()) + " arguments, got " +
                          std::to_string(call.args.size()));

        /* Evaluate arguments and track writeback targets */
        std::vector<int> arg_vals;
        std::vector<std::string> arg_var_names;
        std::vector<int> arg_arr_indices;  /* 0 = not array */
        for (size_t i = 0; i < call.args.size(); i++) {
            arg_vals.push_back(eval_expr(*call.args[i]));
            std::string vname;
            int arr_idx = 0;
            if (is<VarRefNode>(*call.args[i])) {
                vname = as<VarRefNode>(*call.args[i]).name;
            } else if (is<FuncCallNode>(*call.args[i])) {
                const auto &fc = as<FuncCallNode>(*call.args[i]);
                if (fc.args.size() == 1) {
                    Variable *av = scope_find(fc.name);
                    if (av && av->is_array()) {
                        vname = fc.name;
                        arr_idx = eval_expr(*fc.args[0]);
                    }
                }
            }
            arg_var_names.push_back(vname);
            arg_arr_indices.push_back(arr_idx);
        }

        /* Push new scope */
        Scope sub_scope;
        sub_scope.parent = global_scope_.get();
        Scope *caller_scope = current_scope_;
        current_scope_ = &sub_scope;

        for (size_t i = 0; i < sdef.params.size(); i++) {
            scope_set(sdef.params[i].name, arg_vals[i]);
        }

        Flow saved_flow = flow_;
        flow_ = Flow::NORMAL;
        exec_body(sdef.body);
        if (flow_ == Flow::RETURN) flow_ = Flow::NORMAL;
        if (flow_ == Flow::STOP) {
            /* Propagate STOP */
        } else {
            flow_ = saved_flow;
        }

        /* Write back INOUT parameters */
        for (size_t i = 0; i < sdef.params.size(); i++) {
            if (sdef.params[i].intent == IntentType::INOUT) {
                auto pv_it = sub_scope.vars.find(sdef.params[i].name);
                if (pv_it != sub_scope.vars.end() && !arg_var_names[i].empty()) {
                    Variable *cv = nullptr;
                    /* Search in caller scope */
                    Scope *saved = current_scope_;
                    current_scope_ = caller_scope;
                    cv = scope_find(arg_var_names[i]);
                    current_scope_ = saved;
                    if (cv) {
                        if (arg_arr_indices[i] > 0 && cv->is_array()) {
                            int idx = arg_arr_indices[i];
                            if (idx >= 1 && idx <= static_cast<int>(cv->array_data.size())) {
                                cv->array_data[idx - 1] = pv_it->second.value;
                            }
                        } else {
                            cv->value = pv_it->second.value;
                        }
                    }
                }
            }
        }

        current_scope_ = caller_scope;
        return;
    }

    if (is<ReturnNode>(node)) { flow_ = Flow::RETURN; return; }

    if (is<StopNode>(node)) {
        flow_ = Flow::STOP;
        throw StopException{};
    }

    if (is<ExitNode>(node))  { flow_ = Flow::EXIT; return; }
    if (is<CycleNode>(node)) { flow_ = Flow::CYCLE; return; }

    runtime_error(node.line, "Unknown statement type");
}

/* ── Public API ───────────────────────────────────────────────── */

int Interpreter::run() {
    if (!program_) {
        std::cerr << "Error: No PROGRAM block found\n";
        return 1;
    }

    const auto &prog = as<ProgramNode>(*program_);
    try {
        exec_body(prog.stmts);
    } catch (const StopException &) {
        /* STOP was called — normal termination */
    }

    return 0;
}
