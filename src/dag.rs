use std::collections::HashMap;
use lang_c::{visit::Visit as ParserVisit,
    ast::{FunctionDefinition, Expression, CallExpression, Statement, BinaryOperator, BinaryOperatorExpression, Constant}, span::Span};

use crate::utils::{GCed, new_gced};
use crate::lang_defs::CaplanType;

enum IRDAGNodeVType {
    Void,
    Int,
    Dom,
    LinPtr,
    NonlinPtr,
    Label,
    Func
}

struct IRDAGNode {
    vtype: IRDAGNodeVType,
    cons: IRDAGNodeCons,
    // does evaluating this node have side effects?
    side_effects: bool,
    // reverse dependencies
    rev_deps: Vec<GCed<IRDAGNode>>,
    // number of dependencies
    dep_count: u64
}

#[derive(Copy, Clone)]
enum IRDAGNodeIntBinOpType {
    Add, Sub, Mul, Div, Or, Xor, And, // TODO: more
}

#[derive(Copy, Clone)]
enum IRDAGNodeIntUnOpType {
    NEG, NOT 
}

enum IRDAGNodeCons {
    // simple operations on integers; output is also integer
    IntConst(u64),
    IntBinOp(IRDAGNodeIntBinOpType, GCed<IRDAGNode>, GCed<IRDAGNode>),
    IntUnOp(IRDAGNodeIntUnOpType, GCed<IRDAGNode>),
    IncOffset(GCed<IRDAGNode>, GCed<IRDAGNode>),
    // conditional branch
    Branch(GCed<IRDAGNode>, GCed<IRDAGNode>),
    // unconditional jump
    Jump(GCed<IRDAGNode>),
    // in-domain call
    InDomCall(GCed<IRDAGNode>, Vec<GCed<IRDAGNode>>),
    // domain call
    DomCall(GCed<IRDAGNode>, Vec<GCed<IRDAGNode>>),
    // parameter or global variable
    Var(String)
}

impl IRDAGNode {
    pub fn new(vtype: IRDAGNodeVType, cons: IRDAGNodeCons, side_effects: bool) -> Self {
        Self {
            vtype: vtype,
            cons: cons,
            side_effects: side_effects,
            rev_deps: Vec::new(),
            dep_count: 0
        }
    }

    pub fn add_to_rev_deps(&mut self, other: &GCed<IRDAGNode>) {
        self.rev_deps.push(other.clone());
    }

    pub fn inc_dep_count(&mut self) {
        self.dep_count += 1;
    }
}

pub struct IRDAG {
    nodes: Vec<GCed<IRDAGNode>>
}

impl IRDAG {
    pub fn new() -> Self {
        IRDAG {
            nodes: Vec::new()
        }
    }

    fn add_dep(&mut self, a: &GCed<IRDAGNode>, dep: &GCed<IRDAGNode>) {
        (**dep).borrow_mut().add_to_rev_deps(a);
        (**a).borrow_mut().inc_dep_count();
    }

    fn new_int_const(&mut self, v: u64) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntConst(v),
            false
        ));
        self.nodes.push(res.clone());
        res
    }

    fn new_int_binop(&mut self, op_type: IRDAGNodeIntBinOpType, a: &GCed<IRDAGNode>, b: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntBinOp(op_type, a.clone(), b.clone()),
            false
        ));
        self.add_dep(&res, a);
        self.add_dep(&res, b);
        self.nodes.push(res.clone());
        res
    }

    fn new_int_unop(&mut self, op_type: IRDAGNodeIntUnOpType, a: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntUnOp(op_type, a.clone()),
            false
        ));
        self.add_dep(&res, a);
        self.nodes.push(res.clone());
        res
    }

    fn new_var(&mut self, name: &str) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            // TODO: should be looked up, assuming int for now
            IRDAGNodeVType::Int,
            IRDAGNodeCons::Var(String::from(name)),
            false
        ));
        self.nodes.push(res.clone());
        res
    }
}


// a local variable
struct IRDAGVar {
    ty: CaplanType, // do we need this
    node: GCed<IRDAGNode>,
}

pub struct IRDAGBuilder {
    dag: IRDAG,
    locals: Vec<HashMap<String, IRDAGVar>>, // TODO: brute-force implementation
    last_node: GCed<IRDAGNode>,
}

impl IRDAGBuilder {
    pub fn new() -> Self {
        let mut dag = IRDAG::new();
        let dummy_node = dag.new_int_const(0);
        Self {
            dag: dag,
            locals: Vec::new(),
            last_node: dummy_node
        }
    }

    pub fn build(&mut self, ast: &FunctionDefinition, span: &Span) {
        self.visit_function_definition(ast, span);
    }

    // consume the builder and get the dag
    pub fn into_dag(self) -> IRDAG {
        self.dag
    }

    // look up local variable
    fn lookup_local<'a>(&'a self, v: &str) -> Option<&'a IRDAGVar> {
        for local_scope in self.locals.iter().rev() {
            let v = local_scope.get(v);
            if v.is_some() {
                return v;
            }
        }
        None
    }

    // integer binary operator expression
    fn process_int_bin_expr(&mut self, op_type: IRDAGNodeIntBinOpType, expr: &BinaryOperatorExpression) {
        self.visit_expression(&expr.lhs.node,
            &expr.lhs.span);
        let l = self.last_node.clone();
        self.visit_expression(&expr.rhs.node,
            &expr.rhs.span);
        self.last_node = self.dag.new_int_binop(op_type, &l,
            &self.last_node);
    }
}

impl<'ast> ParserVisit<'ast> for IRDAGBuilder {
    fn visit_statement(&mut self, statement: &'ast lang_c::ast::Statement, span: &'ast Span) {
        match statement {
            Statement::Compound(compound_list) => {
                for n in compound_list {
                    self.visit_block_item(&n.node, &n.span);
                }
            }
            Statement::Expression(expr) => {
                if let Some(expr_inner) = expr {
                    self.visit_expression(&expr_inner.node, &expr_inner.span);
                }
            }
            _ => {}
        }
    }

    fn visit_if_statement(&mut self, if_statement: &'ast lang_c::ast::IfStatement, span: &'ast Span) {
        
    }

    fn visit_binary_operator_expression(
            &mut self,
            binary_operator_expression: &'ast lang_c::ast::BinaryOperatorExpression,
            span: &'ast Span,
        ) {
        match binary_operator_expression.operator.node {
            BinaryOperator::Assign => {
                self.visit_expression(&binary_operator_expression.rhs.node, 
                    &binary_operator_expression.rhs.span);
                // TODO: should be either an actual write, or a simple rebind
                // depending on whether we want to do this statically or not
            }
            BinaryOperator::Plus => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Add, binary_operator_expression);
            }
            BinaryOperator::Minus => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Sub, binary_operator_expression);
            }
            BinaryOperator::Multiply => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Mul, binary_operator_expression);
            }
            BinaryOperator::Divide => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Div, binary_operator_expression);
            }
            BinaryOperator::BitwiseAnd => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::And, binary_operator_expression);
            }
            BinaryOperator::BitwiseOr => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Or, binary_operator_expression);
            }
            BinaryOperator::BitwiseXor => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Xor, binary_operator_expression);
            }
            _ => {
                panic!("Unsupported binary operator {:?}", binary_operator_expression.operator.node);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
            // check whether this is a local, param, or global

        // TODO: currently these are assuming rvalue
        if let Some(v) = self.lookup_local(&identifier.name) {
            // found in local
            self.last_node = v.node.clone();
        } else {
            // not found in local
            // either parameter or global
            // we don't need to decide now
            // TODO: actually we need as we need to know the type
            self.last_node = self.dag.new_var(&identifier.name);
        }
    }

    fn visit_call_expression(&mut self, call_expression: &'ast CallExpression, span: &'ast Span) {
        panic!("Call expression unsupported");
    }

    fn visit_constant(&mut self, constant: &'ast lang_c::ast::Constant, span: &'ast Span) {
        match constant {
            Constant::Integer(integer) => {
                self.last_node = self.dag.new_int_const(u64::from_str_radix(integer.number.as_ref(), 10).unwrap());
            }
            _ => {
                panic!("Unsupported constant type {:?}", constant);
            }
        }
    }

}
