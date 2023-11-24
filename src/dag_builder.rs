use std::collections::HashMap;
use std::default;

use crate::lang::{CaplanParam, CaplanGlobalContext};
use crate::lang_defs::CaplanType;
use crate::utils::{GCed, new_gced};
use crate::dag::*;

use lang_c::ast::{UnaryOperator, UnaryOperatorExpression, ForInitializer, Label};
use lang_c::{visit::Visit as ParserVisit,
    ast::{FunctionDefinition, Expression, CallExpression, Statement, 
        BinaryOperator, BinaryOperatorExpression, Constant, DeclaratorKind, 
        DeclarationSpecifier, TypeSpecifier, Initializer}, span::Span};


struct LocalVarInfo {
    ty: CaplanType,
    last_access: Option<GCed<IRDAGNode>>,
    last_write: Option<GCed<IRDAGNode>>,
}

impl LocalVarInfo {
    fn new(ty: CaplanType) -> Self {
        Self {
            ty: ty,
            last_access: None,
            last_write: None
        }
    }
}

struct SwitchTargetInfo<'ast> {
    targets: Vec<(&'ast Expression, &'ast Span, GCed<IRDAGNode>)>,
    default_target: Option<GCed<IRDAGNode>>
}

impl<'ast> SwitchTargetInfo<'ast> {
    fn new() -> Self {
        Self {
            targets: Vec::new(),
            default_target: None
        }
    }
}

pub struct IRDAGBuilder<'ast> {
    dag: IRDAG,
    locals: HashMap<String, LocalVarInfo>, // TODO: brute-force implementation, no nested scope yet
    last_node: GCed<IRDAGNode>,
    last_lval: Option<IRDAGLVal>, // let's say the lvalue can only be an identifier for now
    is_lval: bool, // currently in an lvalue
    decl_type: Option<CaplanType>,
    decl_id_name: Option<String>,
    break_targets: Vec<GCed<IRDAGNode>>,
    continue_targets: Vec<GCed<IRDAGNode>>,
    switch_target_info: Vec<SwitchTargetInfo<'ast>>,
    globals: &'ast CaplanGlobalContext,
    last_func_ident: Option<String>
}

impl<'ast> IRDAGBuilder<'ast> {
    pub fn new(globals: &'ast CaplanGlobalContext) -> Self {
        let mut dag = IRDAG::new();
        let init_label = dag.new_label();
        dag.place_label_node(&init_label);
        Self {
            dag: dag,
            locals: HashMap::new(),
            last_node: init_label,
            last_lval: None,
            is_lval: false,
            decl_type: None,
            decl_id_name: None,
            break_targets: Vec::new(),
            continue_targets: Vec::new(),
            switch_target_info: Vec::new(),
            globals: globals,
            last_func_ident: None
        }
    }

    pub fn build(&mut self, ast: &'ast FunctionDefinition, span: &'ast Span, 
                params: &[CaplanParam]) {
        for param in params.iter() {
            self.locals.insert(param.name.clone(), LocalVarInfo::new(param.ty.clone()));
        }
        self.visit_function_definition(ast, span);
        // self.dag.pretty_print();
    }

    // consume the builder and get the dag
    pub fn into_dag(self) -> IRDAG {
        self.dag
    }

    // look up local variable
    fn lookup_local<'a>(&'a mut self, v: &str) -> Option<&'a mut LocalVarInfo> {
        self.locals.get_mut(v)
    }

    fn push_loop_info(&mut self, break_target: &GCed<IRDAGNode>, continue_target: &GCed<IRDAGNode>) {
        self.break_targets.push(break_target.clone());
        self.continue_targets.push(continue_target.clone());
    }

    fn pop_loop_info(&mut self) {
        self.break_targets.pop().unwrap();
        self.continue_targets.pop().unwrap();
    }

    // integer binary operator expression
    fn process_int_bin_expr(&mut self, op_type: IRDAGNodeIntBinOpType, expr: &'ast BinaryOperatorExpression) {
        self.visit_expression(&expr.lhs.node,
            &expr.lhs.span);
        let l = self.last_node.clone();
        self.visit_expression(&expr.rhs.node,
            &expr.rhs.span);
        self.last_node = self.dag.new_int_binop(op_type, &l,
            &self.last_node);
    }

    // integer unary operator expression
    fn process_int_un_expr(&mut self, op_type: IRDAGNodeIntUnOpType, expr: &'ast UnaryOperatorExpression) {
        self.visit_expression(&expr.operand.node, &expr.operand.span);
        self.last_node = self.dag.new_int_unop(op_type, &self.last_node);
    }

    fn write_var(&mut self, name: &str, node: &GCed<IRDAGNode>) {
        let v = self.lookup_local(name).unwrap();
        if let Some(last_access) = v.last_access.as_ref() {
            // this current access must wait until after the previous access completes
            IRDAG::add_dep(node, last_access);
        }
        v.last_access = Some(node.clone());
        v.last_write = Some(node.clone());
    }

    fn read_var(&mut self, name: &str, node: &GCed<IRDAGNode>) {
        let v = self.lookup_local(name).unwrap();
        if let Some(last_write) = v.last_write.as_ref() {
            IRDAG::add_dep(node, last_write);
        }
        v.last_access = Some(node.clone());
    }

    fn new_block_reset(&mut self) {
        for (_, local_var_mut) in self.locals.iter_mut() {
            local_var_mut.last_access = None;
            local_var_mut.last_write = None;
        }
    }
}

impl<'ast> ParserVisit<'ast> for IRDAGBuilder<'ast> {
    fn visit_if_statement(&mut self, if_statement: &'ast lang_c::ast::IfStatement, span: &'ast Span) {
        self.visit_expression(&if_statement.condition.node, &if_statement.condition.span);
        let cond = self.last_node.clone();
        let label_then = self.dag.new_label();
        let label_taken = self.dag.new_label();
        let branch_node = self.dag.new_branch(&label_taken, &cond);
        self.new_block_reset();
        let _ = self.dag.new_jump(&label_then);
        self.new_block_reset();
        self.dag.place_label_node(&label_taken);
        self.visit_statement(&if_statement.then_statement.node, &if_statement.then_statement.span);
        if let Some(else_statement_node) = if_statement.else_statement.as_ref() {
            let label_end = self.dag.new_label();
            let _ = self.dag.new_jump(&label_end);
            self.new_block_reset();
            self.dag.place_label_node(&label_then);
            self.visit_statement(&else_statement_node.node, &else_statement_node.span);
            self.new_block_reset();
            self.dag.place_label_node(&label_end);
        } else {
            self.new_block_reset();
            self.dag.place_label_node(&label_then);
        }
        self.last_node = branch_node;
    }

    fn visit_while_statement(&mut self, while_statement: &'ast lang_c::ast::WhileStatement, span: &'ast Span) {
        let label_start = self.dag.new_label();
        let label_taken = self.dag.new_label();
        let label_end = self.dag.new_label();
        self.push_loop_info(&label_end, &label_start);
        self.new_block_reset();
        self.dag.place_label_node(&label_start);
        self.visit_expression(&while_statement.expression.node, &while_statement.expression.span);
        let cond = self.last_node.clone();
        let branch_node = self.dag.new_branch(&label_taken, &cond);
        self.new_block_reset();
        let _ = self.dag.new_jump(&label_end);
        self.new_block_reset();
        self.dag.place_label_node(&label_taken);
        self.visit_statement(&while_statement.statement.node, &while_statement.statement.span);
        let _ = self.dag.new_jump(&label_start);
        self.new_block_reset();
        self.dag.place_label_node(&label_end);
        self.last_node = branch_node;
        self.pop_loop_info();
    }

    fn visit_static_assert(&mut self, static_assert: &'ast lang_c::ast::StaticAssert, span: &'ast Span) {
        panic!("Static assertion not supported");
    }

    fn visit_for_statement(&mut self, for_statement: &'ast lang_c::ast::ForStatement, span: &'ast Span) {
        let label_start = self.dag.new_label();
        let label_end = self.dag.new_label();
        let label_cont = self.dag.new_label();

        self.push_loop_info(&label_end, &label_cont);

        self.visit_for_initializer(&for_statement.initializer.node, &for_statement.initializer.span);
        self.new_block_reset();
        self.dag.place_label_node(&label_start);
        if let Some(cond_node) = for_statement.condition.as_ref() {
            self.visit_expression(&cond_node.node, &cond_node.span);
            let label_taken = self.dag.new_label();
            self.dag.new_branch(&label_taken, &self.last_node);
            self.new_block_reset();
            self.dag.new_jump(&label_end);
            self.new_block_reset();
            self.dag.place_label_node(&label_taken);
        }

        self.visit_statement(&for_statement.statement.node, &for_statement.statement.span);
        self.new_block_reset();
        self.dag.place_label_node(&label_cont);
        if let Some(step_node) = for_statement.step.as_ref() {
            self.visit_expression(&step_node.node, &step_node.span);
        }
        self.dag.new_jump(&label_start);
        self.new_block_reset();
        self.dag.place_label_node(&label_end);
        
        self.pop_loop_info();
    }

    fn visit_do_while_statement(
            &mut self,
            do_while_statement: &'ast lang_c::ast::DoWhileStatement,
            span: &'ast Span,
        ) {
        let label_start = self.dag.new_label();
        let label_cont = self.dag.new_label();
        let label_end = self.dag.new_label();

        self.push_loop_info(&label_end, &label_cont);

        self.new_block_reset();
        self.dag.place_label_node(&label_start);
        self.visit_statement(&do_while_statement.statement.node, &do_while_statement.statement.span);
        self.new_block_reset();
        self.dag.place_label_node(&label_cont);
        self.visit_expression(&do_while_statement.expression.node, &do_while_statement.expression.span);
        self.dag.new_branch(&label_start, &self.last_node);
        self.new_block_reset();
        self.dag.place_label_node(&label_end);

        self.pop_loop_info();
    }

    // TODO: use switch node type instead and delay this to codegen
    fn visit_switch_statement(
            &mut self,
            switch_statement: &'ast lang_c::ast::SwitchStatement,
            span: &'ast Span,
        ) {
        // TODO: because the current basic block organisation is stupid, we do weird stuff here
        let label_skip_expr = self.dag.new_label();
        let label_skip_stmt = self.dag.new_label();

        self.switch_target_info.push(SwitchTargetInfo::new());
        self.break_targets.push(label_skip_expr.clone());

        self.dag.new_jump(&label_skip_stmt);
        self.new_block_reset();
        self.visit_statement(&switch_statement.statement.node, &switch_statement.statement.span);
        self.dag.new_jump(&label_skip_expr);
        self.new_block_reset();
        self.dag.place_label_node(&label_skip_stmt);

        self.break_targets.pop().unwrap();
        let switch_targets = self.switch_target_info.pop().unwrap();
        self.visit_expression(&switch_statement.expression.node, &switch_statement.expression.span);
        let val = self.last_node.clone();

        for (expr, expr_span, target) in switch_targets.targets.iter() {
            self.visit_expression(expr, expr_span);
            let cmp_res = self.dag.new_int_binop(IRDAGNodeIntBinOpType::Eq, &val, &self.last_node);
            self.dag.new_branch(target, &cmp_res);
            self.new_block_reset();
        }

        if let Some(default_target) = switch_targets.default_target.as_ref() {
            self.dag.new_jump(default_target);
            self.new_block_reset();
        }

        self.new_block_reset();
        self.dag.place_label_node(&label_skip_expr);
    }

    fn visit_label(&mut self, label: &'ast lang_c::ast::Label, span: &'ast Span) {
        match label {
            Label::Case(expr) => {
                let label = self.dag.new_label();
                self.new_block_reset();
                self.dag.place_label_node(&label);

                self.switch_target_info.last_mut().unwrap().targets.push((&expr.node, &expr.span, label));
            }
            Label::Default => {
                let label = self.dag.new_label();
                self.new_block_reset();
                self.dag.place_label_node(&label);

                let last_switch_target_info = self.switch_target_info.last_mut().unwrap();
                assert!(last_switch_target_info.default_target.is_none(), "Duplicate default");
                last_switch_target_info.default_target = Some(label);
            }
            _ => {
                panic!("Unsupported label type {:?}", label);
            }
        }
    }

    fn visit_statement(&mut self, statement: &'ast Statement, span: &'ast Span) {
        match statement {
            Statement::Continue => {
                self.dag.new_jump(&self.continue_targets.last().unwrap());
                self.new_block_reset();
            }
            Statement::Break => {
                self.dag.new_jump(&self.break_targets.last().unwrap());
                self.new_block_reset();
            }
            Statement::Expression(None) => (),
            Statement::Expression(Some(expr_node)) => self.visit_expression(&expr_node.node, &expr_node.span),
            Statement::If(if_stmt) => self.visit_if_statement(&if_stmt.node, &if_stmt.span),
            Statement::For(for_stmt) => self.visit_for_statement(&for_stmt.node, &for_stmt.span),
            Statement::While(while_stmt) => self.visit_while_statement(&while_stmt.node, &while_stmt.span),
            Statement::DoWhile(do_while_stmt) => self.visit_do_while_statement(&do_while_stmt.node, &do_while_stmt.span),
            Statement::Return(expr) => {
                let ret_val_node = expr.as_ref().map(|ret_val_expr| {
                    self.visit_expression(&ret_val_expr.node, &ret_val_expr.span);
                    self.last_node.clone()
                });
                self.dag.new_indom_return(ret_val_node);
                self.new_block_reset();
            }
            Statement::Compound(compound) =>
                compound.iter().for_each(|block_item_node| self.visit_block_item(&block_item_node.node, &block_item_node.span)),
            Statement::Labeled(labeled_stmt) => self.visit_labeled_statement(&labeled_stmt.node, &labeled_stmt.span),
            Statement::Switch(switch_stmt) => self.visit_switch_statement(&switch_stmt.node, &switch_stmt.span),
            _ => panic!("Unsupported statement {:?}", statement)
        }
    }

    fn visit_unary_operator_expression(
            &mut self,
            unary_operator_expression: &'ast lang_c::ast::UnaryOperatorExpression,
            span: &'ast Span,
        ) {
        match unary_operator_expression.operator.node {
            UnaryOperator::Negate => self.process_int_un_expr(IRDAGNodeIntUnOpType::Negate, unary_operator_expression),
            UnaryOperator::Minus => self.process_int_un_expr(IRDAGNodeIntUnOpType::Neg, unary_operator_expression),
            UnaryOperator::Complement => self.process_int_un_expr(IRDAGNodeIntUnOpType::Not, unary_operator_expression),
            UnaryOperator::Address => {
                let old_is_lval = self.is_lval;
                self.is_lval = true;
                self.visit_expression(&unary_operator_expression.operand.node, &unary_operator_expression.operand.span);
                self.is_lval = old_is_lval;

                self.last_node = self.dag.new_address_of(self.last_lval.take().unwrap());
            }
            UnaryOperator::Indirection => {
                let old_is_lval = self.is_lval;
                self.is_lval = false;
                self.visit_expression(&unary_operator_expression.operand.node, &unary_operator_expression.operand.span);
                self.is_lval = old_is_lval;

                if self.is_lval {
                    assert!(self.last_lval.is_none(), "last_lval already taken");
                    self.last_lval = Some(IRDAGLVal::AddressIndirection(self.last_node.clone()));
                } else {
                    // not lval, just read
                    self.last_node = self.dag.new_read_indirection(self.last_node.clone());
                }
            }
            _ => panic!("Unsupported unary operator {:?}", unary_operator_expression.operator.node)
        }
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
                let lval = self.last_lval.take().unwrap();
                self.last_node = self.dag.new_write(lval.clone(), &self.last_node);
                if let IRDAGLVal::Identifier(ident) = lval {
                    self.write_var(&ident, &self.last_node.clone());
                }
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
            BinaryOperator::NotEquals => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::NEq, binary_operator_expression);
            }
            BinaryOperator::Less => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::LessThan, binary_operator_expression);
            }
            BinaryOperator::LessOrEqual => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::LessEq, binary_operator_expression);
            }
            BinaryOperator::Greater => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::GreaterThan, binary_operator_expression);
            }
            BinaryOperator::GreaterOrEqual => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::GreaterEq, binary_operator_expression);
            }
            // TODO: we use the same for bitwise ops for now
            BinaryOperator::LogicalAnd => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::And, binary_operator_expression);
            }
            BinaryOperator::LogicalOr => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Or, binary_operator_expression);
            }
            _ => {
                panic!("Unsupported binary operator {:?}", binary_operator_expression.operator.node);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
        // check whether this is a local, param, or global
        if self.is_lval {
            self.last_lval = Some(IRDAGLVal::Identifier(identifier.name.clone()));
        } else if self.globals.func_decls.contains(&identifier.name) {
            assert!(self.last_func_ident.is_none());
            self.last_func_ident = Some(identifier.name.clone());
        } else {
            self.last_node = self.dag.new_read(identifier.name.clone());
            self.read_var(&identifier.name, &self.last_node.clone());
        }
    }

    fn visit_call_expression(&mut self, call_expression: &'ast CallExpression, span: &'ast Span) {
        self.visit_expression(&call_expression.callee.node, &call_expression.callee.span);
        let func_name = self.last_func_ident.take().unwrap();
        let args = Vec::from_iter(call_expression.arguments.iter().map(
            |arg_node| {
                self.visit_expression(&arg_node.node, &arg_node.span);
                self.last_node.clone()
            }
        ));
        self.last_node = self.dag.new_indom_call(&func_name, args);
        self.new_block_reset();
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
                self.decl_type = CaplanType::from_ast_type(&type_specifier.node, self.globals);
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
        // add this to local
        let name = self.decl_id_name.take().unwrap();
        self.locals.insert(name.clone(),
            LocalVarInfo::new(self.decl_type.as_ref().unwrap().clone()));
        if let Some(initializer_node) = init_declarator.initializer.as_ref() {
            self.visit_initializer(&initializer_node.node, &initializer_node.span);
            self.last_node = self.dag.new_write(IRDAGLVal::Identifier(name.clone()), &self.last_node);
            self.write_var(&name, &self.last_node.clone());
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