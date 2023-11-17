use std::borrow::BorrowMut;
use lang_c::{visit::Visit as ParserVisit, ast::FunctionDefinition, span::Span};

use crate::utils::{GCed, new_gced};

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
    DomCall(GCed<IRDAGNode>, Vec<GCed<IRDAGNode>>)
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

    pub fn from_ast(ast: &FunctionDefinition, span: &Span) -> Self {
        let mut res = Self::new();
        res.visit_function_definition(ast, span);
        res
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
}

impl<'ast> ParserVisit<'ast> for IRDAG {
    fn visit_function_definition(
            &mut self,
            function_definition: &'ast FunctionDefinition,
            span: &'ast Span,
        ) {
    }
}
