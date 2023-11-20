use std::collections::HashMap;
use lang_c::{visit::Visit as ParserVisit,
    ast::{FunctionDefinition, Expression, CallExpression, Statement, 
        BinaryOperator, BinaryOperatorExpression, Constant, DeclaratorKind, 
        DeclarationSpecifier, TypeSpecifier, Initializer}, span::Span};

use crate::utils::{GCed, new_gced};
use crate::lang_defs::CaplanType;

#[derive(Copy, Clone, Debug)]
enum IRDAGNodeVType {
    Void,
    Int,
    Dom,
    LinPtr,
    NonlinPtr,
    Label,
    Func
}

#[derive(Debug)]
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

#[derive(Copy, Clone, Debug)]
enum IRDAGNodeIntBinOpType {
    Add, Sub, Mul, Div, Or, Xor, And,
    Eq, LessThan, GreaterThan, LessEq, GreaterEq, NEq,
    // TODO: more
}

#[derive(Copy, Clone, Debug)]
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
    // in-domain return
    InDomReturn,
    // domain return, a capability needed
    DomReturn(GCed<IRDAGNode>),
    // local, parameter or global variable
    Read(String),
    // write to local, parameter or global variable
    Write(String, GCed<IRDAGNode>),
    // label of a specific basic block
    // Label(None) is a label that has not been placed
    Label(Option<usize>)
}

impl std::fmt::Debug for IRDAGNodeCons {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntConst(arg0) => f.debug_tuple("IntConst").field(arg0).finish(),
            Self::IntBinOp(arg0, _, _) => f.debug_tuple("IntBinOp").field(arg0).finish(),
            Self::IntUnOp(arg0, _) => f.debug_tuple("IntUnOp").field(arg0).finish(),
            Self::IncOffset(_, _) => write!(f, "IncOffset"),
            Self::Branch(_, _) => write!(f, "Branch"),
            Self::Jump(_) => write!(f, "Jump"),
            Self::InDomCall(_, _) => write!(f, "InDomCall"),
            Self::DomCall(_, _) => write!(f, "DomCall"),
            Self::InDomReturn => write!(f, "InDomReturn"),
            Self::DomReturn(_) => write!(f, "DomReturn"),
            Self::Read(arg0) => f.debug_tuple("Read").field(arg0).finish(),
            Self::Write(arg0, _) => f.debug_tuple("Write").field(arg0).finish(),
            Self::Label(arg0) => f.debug_tuple("Label").field(arg0).finish(),
        }
    }
}

impl IRDAGNodeCons {
    fn is_control_flow(&self) -> bool {
        match self {
            IRDAGNodeCons::Branch(_, _) => true,
            IRDAGNodeCons::Jump(_) => true,
            IRDAGNodeCons::InDomCall(_, _) => true,
            IRDAGNodeCons::DomCall(_, _) => true,
            _ => {
                false
            }
        }
    }
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

struct IRDAGBlock {
    dag: Vec<GCed<IRDAGNode>>,
    exit_node: Option<GCed<IRDAGNode>> /* the control flow node that jumps to another basic block */
}

impl IRDAGBlock {
    fn new() -> Self {
        Self {
            dag: Vec::new(),
            exit_node: None
        }
    }
}

pub struct IRDAG {
    blocks: Vec<IRDAGBlock>, // DAGs are separate per basic block
}

impl IRDAG {
    pub fn new() -> Self {
        IRDAG {
            blocks: vec![IRDAGBlock::new()],
        }
    }

    fn add_dep(a: &GCed<IRDAGNode>, dep: &GCed<IRDAGNode>) {
        (**dep).borrow_mut().add_to_rev_deps(a);
        (**a).borrow_mut().inc_dep_count();
    }

    fn add_nonlabel_node(&mut self, node: &GCed<IRDAGNode>) {
        let last_block = self.blocks.last_mut().unwrap();
        assert!(last_block.exit_node.is_none()); // we should not have seen an exit node yet
        last_block.dag.push(node.clone());
        if node.borrow().cons.is_control_flow() {
            last_block.exit_node = Some(node.clone());
            self.new_block();
        }
    }

    /* Returns the index of the new basic block */
    fn new_block(&mut self) -> usize {
        self.blocks.push(IRDAGBlock::new());
        self.blocks.len() - 1
    }

    fn current_block_id(&self) -> usize {
        assert!(!self.blocks.is_empty());
        self.blocks.len() - 1
    }

    fn place_label_node(&mut self, label_node: &GCed<IRDAGNode>) {
        let blk_id = 
            if self.blocks.last().unwrap().dag.is_empty() {
                self.current_block_id()
            } else {
                self.new_block()
            };
        assert!(self.blocks.last().unwrap().dag.is_empty()); // must be the first node to add
        let mut label_node_ref = label_node.borrow_mut();
        if let IRDAGNodeCons::Label(None) = label_node_ref.cons {
            label_node_ref.cons = IRDAGNodeCons::Label(Some(blk_id));
        }
    }

    fn new_int_const(&mut self, v: u64) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntConst(v),
            false
        ));
        self.add_nonlabel_node(&res);
        res
    }

    fn new_int_binop(&mut self, op_type: IRDAGNodeIntBinOpType, a: &GCed<IRDAGNode>, b: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntBinOp(op_type, a.clone(), b.clone()),
            false
        ));
        Self::add_dep(&res, a);
        Self::add_dep(&res, b);
        self.add_nonlabel_node(&res);
        res
    }

    fn new_int_unop(&mut self, op_type: IRDAGNodeIntUnOpType, a: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntUnOp(op_type, a.clone()),
            false
        ));
        Self::add_dep(&res, a);
        self.add_nonlabel_node(&res);
        res
    }

    fn new_read(&mut self, name: String) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            // TODO: should be looked up, assuming int for now
            IRDAGNodeVType::Int,
            IRDAGNodeCons::Read(name),
            false
        ));
        self.add_nonlabel_node(&res);
        res
    }

    fn new_write(&mut self, name: String, v: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            v.borrow().vtype,
            IRDAGNodeCons::Write(name, v.clone()),
            false
        ));
        Self::add_dep(&res, v);
        self.add_nonlabel_node(&res);
        res
    }

    fn new_label(&mut self) -> GCed<IRDAGNode> {
        // label nodes are not added to the list until they are placed
        new_gced(IRDAGNode::new(
            IRDAGNodeVType::Label,
            IRDAGNodeCons::Label(None),
            true
        ))
    }

    fn new_branch(&mut self, target: &GCed<IRDAGNode>, cond: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Void,
            IRDAGNodeCons::Branch(target.clone(), cond.clone()),
            true
        ));
        Self::add_dep(&res, cond);
        self.add_nonlabel_node(&res);
        res
    }

    fn new_jump(&mut self, target: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            IRDAGNodeVType::Void,
            IRDAGNodeCons::Jump(target.clone()),
            true
        ));
        self.add_nonlabel_node(&res);
        res
    }

    pub fn pretty_print(&self) {
        eprintln!("IRDAG with {} blocks", self.blocks.len());
        // output to dot format
        println!("strict digraph {{");
        for block in self.blocks.iter() {
            for node in block.dag.iter() {
                let node_addr = (&*node.borrow()) as *const IRDAGNode as *const () as usize;
                let node_label = format!("{} ({:?}: {:?})", node_addr, node.borrow().cons, node.borrow().vtype).replace("\"", "\\\"");
                println!("\"{}\";", node_label);
                for rev_dep in node.borrow().rev_deps.iter() {
                    let rev_dep_addr = (&*rev_dep.borrow()) as *const IRDAGNode as *const () as usize;
                    let rev_dep_label = format!("{} ({:?}: {:?})", rev_dep_addr, rev_dep.borrow().cons, rev_dep.borrow().vtype).replace("\"", "\\\"");
                    println!("\"{}\" -> \"{}\";", node_label, rev_dep_label);
                }
            }
        }
        println!("}}");
    }
}


// a local variable
struct IRDAGVar {
    ty: CaplanType, // do we need this
}

pub struct IRDAGBuilder {
    dag: IRDAG,
    locals: HashMap<String, CaplanType>, // TODO: brute-force implementation, no nested scope yet
    last_node: GCed<IRDAGNode>,
    lval_id_name: Option<String>, // let's say the lvalue can only be an identifier for now
    is_lval: bool, // currently in an lvalue
    decl_type: Option<CaplanType>,
    decl_id_name: Option<String>
}

impl IRDAGBuilder {
    pub fn new() -> Self {
        let mut dag = IRDAG::new();
        let init_label = dag.new_label();
        dag.place_label_node(&init_label);
        Self {
            dag: dag,
            locals: HashMap::new(),
            last_node: init_label,
            lval_id_name: None,
            is_lval: false,
            decl_type: None,
            decl_id_name: None
        }
    }

    pub fn build(&mut self, ast: &FunctionDefinition, span: &Span) {
        self.visit_function_definition(ast, span);
        self.dag.pretty_print();
    }

    // consume the builder and get the dag
    pub fn into_dag(self) -> IRDAG {
        self.dag
    }

    // look up local variable
    fn lookup_local<'a>(&'a self, v: &str) -> Option<&'a CaplanType> {
        self.locals.get(v)
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
    fn visit_if_statement(&mut self, if_statement: &'ast lang_c::ast::IfStatement, span: &'ast Span) {
        self.visit_expression(&if_statement.condition.node, &if_statement.condition.span);
        let cond = self.last_node.clone();
        let label_then = self.dag.new_label();
        let label_taken = self.dag.new_label();
        let branch_node = self.dag.new_branch(&label_taken, &cond);
        let _ = self.dag.new_jump(&label_then);
        self.dag.place_label_node(&label_taken);
        self.visit_statement(&if_statement.then_statement.node, &if_statement.then_statement.span);
        if let Some(else_statement_node) = if_statement.else_statement.as_ref() {
            let label_end = self.dag.new_label();
            let _ = self.dag.new_jump(&label_end);
            self.dag.place_label_node(&label_then);
            self.visit_statement(&else_statement_node.node, &else_statement_node.span);
            self.dag.place_label_node(&label_end);
        } else {
            self.dag.place_label_node(&label_then);
        }
        self.last_node = branch_node;
    }

    fn visit_while_statement(&mut self, while_statement: &'ast lang_c::ast::WhileStatement, span: &'ast Span) {
        self.visit_expression(&while_statement.expression.node, &while_statement.expression.span);
        let label_start = self.dag.new_label();
        let label_taken = self.dag.new_label();
        let label_end = self.dag.new_label();
        self.dag.place_label_node(&label_start);
        let cond = self.last_node.clone();
        let branch_node = self.dag.new_branch(&cond, &label_taken);
        let _ = self.dag.new_jump(&label_end);
        self.dag.place_label_node(&label_taken);
        self.visit_statement(&while_statement.statement.node, &while_statement.statement.span);
        let _ = self.dag.new_jump(&label_start);
        self.dag.place_label_node(&label_end);
        self.last_node = branch_node;
    }

    fn visit_binary_operator_expression(
            &mut self,
            binary_operator_expression: &'ast lang_c::ast::BinaryOperatorExpression,
            span: &'ast Span,
        ) {
        match binary_operator_expression.operator.node {
            BinaryOperator::Assign => {
                let old_is_lval = self.is_lval;
                self.is_lval = true;
                self.visit_expression(&binary_operator_expression.lhs.node,
                    &binary_operator_expression.lhs.span);
                self.is_lval = false;
                self.visit_expression(&binary_operator_expression.rhs.node, 
                    &binary_operator_expression.rhs.span);
                self.is_lval = old_is_lval;
                eprintln!("Assignment {}", self.lval_id_name.as_ref().unwrap());
                self.last_node = self.dag.new_write(self.lval_id_name.take().unwrap(), &self.last_node);
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
            BinaryOperator::Equals => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Eq, binary_operator_expression);
            }
            _ => {
                panic!("Unsupported binary operator {:?}", binary_operator_expression.operator.node);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
            // check whether this is a local, param, or global

        if self.is_lval {
            self.lval_id_name = Some(identifier.name.clone());
        } else {
            self.last_node = self.dag.new_read(identifier.name.clone());
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

    fn visit_declaration_specifier(
            &mut self,
            declaration_specifier: &'ast lang_c::ast::DeclarationSpecifier,
            span: &'ast Span,
        ) {
        match declaration_specifier {
            DeclarationSpecifier::TypeSpecifier(type_specifier) => {
                assert!(self.decl_type.is_none()); // can't have multiple type specifiers
                self.decl_type = match &type_specifier.node {
                    TypeSpecifier::Void => Some(CaplanType::Void),
                    TypeSpecifier::Bool => Some(CaplanType::Int),
                    TypeSpecifier::Char => Some(CaplanType::Int),
                    TypeSpecifier::Short => Some(CaplanType::Int),
                    TypeSpecifier::Int => Some(CaplanType::Int),
                    TypeSpecifier::Long => Some(CaplanType::Int),
                    TypeSpecifier::Signed => Some(CaplanType::Int),
                    TypeSpecifier::Unsigned => Some(CaplanType::Int),
                    _ => None
                }
            }
            _ => {}
        }
    }

    fn visit_declarator(&mut self, declarator: &'ast lang_c::ast::Declarator, span: &'ast Span) {
        match &declarator.kind.node {
            DeclaratorKind::Identifier(id_node) => {
                assert!(self.decl_id_name.is_none()); // can't have multiple names
                self.decl_id_name = Some(id_node.node.name.clone());
            }
            _ => {}
        }
    }

    fn visit_initializer(&mut self, initializer: &'ast lang_c::ast::Initializer, span: &'ast Span) {
        match initializer {
            Initializer::Expression(expr) => {
                self.visit_expression(&expr.node, &expr.span);
            }
            _ => {
                panic!("Unsupported initializer {:?}", initializer);
            }
        }
    }

    fn visit_init_declarator(&mut self, init_declarator: &'ast lang_c::ast::InitDeclarator, span: &'ast Span) {
        assert!(self.decl_id_name.is_none());
        self.visit_declarator(&init_declarator.declarator.node, &init_declarator.declarator.span);
        assert!(self.decl_id_name.is_some());
        // add this to local
        self.locals.insert(self.decl_id_name.as_ref().unwrap().clone(), self.decl_type.as_ref().unwrap().clone());
        if let Some(initializer_node) = init_declarator.initializer.as_ref() {
            self.visit_initializer(&initializer_node.node, &initializer_node.span);
            self.last_node = self.dag.new_write(self.decl_id_name.take().unwrap(), &self.last_node);
        }
        self.decl_id_name = None;
    }

    fn visit_declaration(&mut self, declaration: &'ast lang_c::ast::Declaration, span: &'ast Span) {
        assert!(self.decl_type.is_none()); // not already in a declaration
        for decl_specifier in declaration.specifiers.iter() {
            self.visit_declaration_specifier(&decl_specifier.node, &decl_specifier.span);
        }
        assert!(self.decl_type.is_some()); // we should have got a type
        for init_decl in declaration.declarators.iter() {
            self.visit_init_declarator(&init_decl.node, &init_decl.span);
        }
        self.decl_type = None;
    }

    fn visit_function_definition(
            &mut self,
            function_definition: &'ast FunctionDefinition,
            span: &'ast Span,
        ) {
        self.visit_statement(&function_definition.statement.node, &function_definition.statement.span);
    }

}
