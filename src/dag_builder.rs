use std::collections::HashMap;
use std::default;

use crate::lang::{CaplanParam, CaplanGlobalContext};
use crate::lang_defs::CaplanType;
use crate::utils::{GCed, new_gced};
use crate::dag::*;

use lang_c::ast::{UnaryOperator, UnaryOperatorExpression, ForInitializer, Label, MemberOperator};
use lang_c::{visit::Visit as ParserVisit,
    ast::{FunctionDefinition, Expression, CallExpression, Statement, 
        BinaryOperator, BinaryOperatorExpression, Constant, DeclaratorKind, 
        DeclarationSpecifier, TypeSpecifier, Initializer}, span::Span};


struct IRDAGNamedMemLocInfo {
    last_access: Option<GCed<IRDAGNode>>,
    last_write: Option<GCed<IRDAGNode>>
}

impl IRDAGNamedMemLocInfo {
    fn new() -> Self {
        Self {
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

// only used for building the dag

pub struct IRDAGBuilder<'ast> {
    dag: IRDAG,
    locals: HashMap<String, CaplanType>, // TODO: brute-force implementation, no nested scope yet
    local_named_locs: HashMap<IRDAGNamedMemLoc, IRDAGNamedMemLocInfo>,
    last_temp_res: Option<IRDAGNodeTempResult>,
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
            local_named_locs: HashMap::new(),
            last_temp_res: None,
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
            self.locals.insert(param.name.clone(), param.ty.clone());
            for offset in (0..param.ty.size()).step_by(8) {
                self.local_named_locs.insert(IRDAGNamedMemLoc { var_name: param.name.clone(), offset: offset }, IRDAGNamedMemLocInfo::new());
            }
        }
        self.visit_function_definition(ast, span);
        // self.dag.pretty_print();
    }

    // consume the builder and get the dag
    pub fn into_dag(self) -> (IRDAG, HashMap<String, CaplanType>) {
        (self.dag, self.locals)
    }

    // look up local variable
    fn lookup_local<'a>(&'a mut self, v: &str) -> Option<&'a mut CaplanType> {
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

    fn read_location(&mut self, loc: &IRDAGMemLoc) -> GCed<IRDAGNode> {
        match loc {
            IRDAGMemLoc::Addr(_) => self.dag.new_read(loc.clone()),
            IRDAGMemLoc::Named(named_mem_loc) => {
                let read_node = self.dag.new_read(loc.clone());
                self.read_named_mem_loc(named_mem_loc, &read_node);
                read_node
            }
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, _, offset_range) => {
                let read_node = self.dag.new_read(loc.clone());
                for offset in offset_range.clone().step_by(8) {
                    let covered_loc = named_mem_loc.clone().with_offset(offset); // TODO: very brute force
                    self.read_named_mem_loc(&covered_loc, &read_node);
                }
                read_node
            }
        }
    }

    fn write_location(&mut self, loc: &IRDAGMemLoc, val: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        match loc {
            IRDAGMemLoc::Addr(_) => self.dag.new_write(loc.clone(), val),
            IRDAGMemLoc::Named(named_mem_loc) => {
                let write_node = self.dag.new_write(loc.clone(), val);
                self.write_named_mem_loc(named_mem_loc, &write_node);
                write_node
            }
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, _, offset_range) => {
                let write_node = self.dag.new_write(loc.clone(), val);
                for offset in offset_range.clone().step_by(8) {
                    let covered_loc = named_mem_loc.clone().with_offset(offset);
                    self.write_named_mem_loc(&covered_loc, &write_node);
                }
                write_node
            }
        }
    }
    
    fn result_to_word(&mut self, res: &IRDAGNodeTempResult) -> Option<GCed<IRDAGNode>> {
        match res {
            IRDAGNodeTempResult::Word(word) => Some(word.clone()),
            IRDAGNodeTempResult::LVal(lval) => {
                if lval.ty.size() > 8 {
                    None
                } else {
                    // read the variable location
                    Some(self.read_location(&lval.loc))
                }
            }
        }
    }

    fn last_temp_res_to_word(&mut self) -> Option<GCed<IRDAGNode>> {
        self.last_temp_res.take().and_then(|x| self.result_to_word(&x))
    }

    fn last_temp_res_to_indexable_lval(&mut self) -> Option<IRDAGLVal> {
        self.last_temp_res.take().and_then(
            |r| {
                match r {
                    IRDAGNodeTempResult::LVal(lval) => {
                        let ty = lval.ty;
                        let loc = lval.loc;
                        match ty {
                            CaplanType::Array(inner_type, _) =>
                                Some(IRDAGLVal { ty: *inner_type, loc: loc }),
                            _ => None
                        }
                    }
                    IRDAGNodeTempResult::Word(word) => {
                        let vtype = word.borrow().vtype.clone();
                        match vtype {
                            IRDAGNodeVType::RawPtr(inner_type) =>
                                Some(IRDAGLVal { ty: inner_type, loc: IRDAGMemLoc::Addr(word) }),
                            _ => None
                        }
                    }
                }
            }
        )
    }

    // integer binary operator expression
    fn process_int_bin_expr(&mut self, op_type: IRDAGNodeIntBinOpType, expr: &'ast BinaryOperatorExpression) {
        self.visit_expression(&expr.lhs.node,
            &expr.lhs.span);
        let l = self.last_temp_res_to_word().unwrap();
        self.visit_expression(&expr.rhs.node,
            &expr.rhs.span);
        let r = self.last_temp_res_to_word().unwrap();
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(self.dag.new_int_binop(op_type, &l, &r)));
    }

    // integer unary operator expression
    fn process_int_un_expr(&mut self, op_type: IRDAGNodeIntUnOpType, expr: &'ast UnaryOperatorExpression) {
        self.visit_expression(&expr.operand.node, &expr.operand.span);
        let r = self.last_temp_res_to_word().unwrap();
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(self.dag.new_int_unop(op_type, &r)));
    }

    fn write_named_mem_loc(&mut self, mem_loc: &IRDAGNamedMemLoc, node: &GCed<IRDAGNode>) {
        let v = self.local_named_locs.get_mut(mem_loc).unwrap();
        if let Some(last_access) = v.last_access.as_ref() {
            // this current access must wait until after the previous access completes
            IRDAG::add_dep(node, last_access);
        }
        v.last_access = Some(node.clone());
        v.last_write = Some(node.clone());
    }

    fn read_named_mem_loc(&mut self, mem_loc: &IRDAGNamedMemLoc, node: &GCed<IRDAGNode>) {
        // eprintln!("{:?}", mem_loc);
        let v = self.local_named_locs.get_mut(mem_loc).unwrap();
        if let Some(last_write) = v.last_write.as_ref() {
            IRDAG::add_dep(node, last_write);
        }
        v.last_access = Some(node.clone());
    }

    fn new_block_reset(&mut self) {
        for (_, local_loc_mut) in self.local_named_locs.iter_mut() {
            local_loc_mut.last_access = None;
            local_loc_mut.last_write = None;
        }
    }

    fn gen_assign(&mut self, lhs: IRDAGNodeTempResult, rhs: IRDAGNodeTempResult) {
        if let IRDAGNodeTempResult::LVal(lval) = lhs {
            let orig_lval = lval.clone();
            let size = lval.ty.size();
            if size > 8 {
                match rhs {
                    IRDAGNodeTempResult::Word(_) => panic!("Size mismatch for assignment"),
                    IRDAGNodeTempResult::LVal(rhs_lval) => {
                        if size != rhs_lval.ty.size() {
                            panic!("Size mismatch for assignment");
                        } else {
                            let mut lhs_loc = lval.loc;
                            let mut rhs_loc = rhs_lval.loc;
                            for _ in (0..size).step_by(8) {
                                let read_res = self.read_location(&rhs_loc);
                                self.write_location(&lhs_loc, &read_res);
                                lhs_loc = lhs_loc.apply_offset(8, &mut self.dag);
                                rhs_loc = rhs_loc.apply_offset(8, &mut self.dag);
                            }
                        }
                    }
                }
            } else {
                let rhs_word = self.result_to_word(&rhs).expect("Size mismatch for assignment");
                self.write_location(&lval.loc, &rhs_word);
            }

            self.last_temp_res = Some(IRDAGNodeTempResult::LVal(orig_lval));
        } else {
            panic!("Requires lval at lhs of assignment");
        }
    }
}

impl<'ast> ParserVisit<'ast> for IRDAGBuilder<'ast> {
    fn visit_if_statement(&mut self, if_statement: &'ast lang_c::ast::IfStatement, span: &'ast Span) {
        self.visit_expression(&if_statement.condition.node, &if_statement.condition.span);
        let cond = self.last_temp_res_to_word().unwrap();
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
    }

    fn visit_while_statement(&mut self, while_statement: &'ast lang_c::ast::WhileStatement, span: &'ast Span) {
        let label_start = self.dag.new_label();
        let label_taken = self.dag.new_label();
        let label_end = self.dag.new_label();
        self.push_loop_info(&label_end, &label_start);
        self.new_block_reset();
        self.dag.place_label_node(&label_start);
        self.visit_expression(&while_statement.expression.node, &while_statement.expression.span);
        let cond = self.last_temp_res_to_word().unwrap();
        let _ = self.dag.new_branch(&label_taken, &cond);
        self.new_block_reset();
        let _ = self.dag.new_jump(&label_end);
        self.new_block_reset();
        self.dag.place_label_node(&label_taken);
        self.visit_statement(&while_statement.statement.node, &while_statement.statement.span);
        let _ = self.dag.new_jump(&label_start);
        self.new_block_reset();
        self.dag.place_label_node(&label_end);
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
            let cond = self.last_temp_res_to_word().unwrap();
            self.dag.new_branch(&label_taken, &cond);
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
        let cond = self.last_temp_res_to_word().unwrap();
        self.dag.new_branch(&label_start, &cond);
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
        let val = self.last_temp_res_to_word().unwrap();

        for (expr, expr_span, target) in switch_targets.targets.iter() {
            self.visit_expression(expr, expr_span);
            let b_res = self.last_temp_res_to_word().unwrap();
            let cmp_res = self.dag.new_int_binop(IRDAGNodeIntBinOpType::Eq, 
                &val, &b_res);
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
                    self.last_temp_res_to_word().unwrap() // TODO: only 8 byte can be returned
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
                self.visit_expression(&unary_operator_expression.operand.node, &unary_operator_expression.operand.span);
                let r = self.last_temp_res.take().unwrap();
                self.last_temp_res = if let IRDAGNodeTempResult::LVal(lval) = r {
                    match lval.loc {
                        IRDAGMemLoc::Addr(addr) => Some(IRDAGNodeTempResult::Word(addr)),
                        IRDAGMemLoc::Named(named) => Some(IRDAGNodeTempResult::Word(self.dag.new_address_of(named, lval.ty))),
                        IRDAGMemLoc::NamedWithDynOffset(named, dyn_offset, offset_range) => {
                            let base_addr = self.dag.new_address_of(named, lval.ty);
                            let addr = self.dag.new_int_binop(IRDAGNodeIntBinOpType::Add, &base_addr, &dyn_offset);
                            Some(IRDAGNodeTempResult::Word(addr))
                        }
                    }
                } else {
                    panic!("Cannot take address of integers");
                };
            }
            UnaryOperator::Indirection => {
                // TODO: some basic optimisation
                self.visit_expression(&unary_operator_expression.operand.node, &unary_operator_expression.operand.span);
                let r = self.last_temp_res_to_word().unwrap();
                let new_type = match &r.borrow().vtype {
                    IRDAGNodeVType::RawPtr(inner_type) => inner_type.clone(),
                    IRDAGNodeVType::Int => CaplanType::Int, // TODO: temporary, should remove this
                    _ => panic!("Invalid type for indirection access")
                };
                self.last_temp_res = Some(IRDAGNodeTempResult::LVal(IRDAGLVal {
                    ty: new_type,
                    loc: IRDAGMemLoc::Addr(r)
                }));
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
                self.visit_expression(&binary_operator_expression.lhs.node,
                    &binary_operator_expression.lhs.span);
                let lhs = self.last_temp_res.take().unwrap();
                self.visit_expression(&binary_operator_expression.rhs.node, 
                    &binary_operator_expression.rhs.span);
                let rhs = self.last_temp_res.take().unwrap();
                self.gen_assign(lhs, rhs);
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
            BinaryOperator::Index => {
                self.visit_expression(&binary_operator_expression.lhs.node, &binary_operator_expression.rhs.span);
                let lhs = self.last_temp_res_to_indexable_lval().unwrap();
                self.visit_expression(&binary_operator_expression.rhs.node, &binary_operator_expression.rhs.span);
                let rhs = self.last_temp_res_to_word().unwrap();
                // take address
                let elem_ty = lhs.ty.clone();
                let elem_size_const = self.dag.new_int_const(elem_ty.size() as u64);
                let offset = self.dag.new_int_binop(
                    IRDAGNodeIntBinOpType::Mul,
                    &elem_size_const, &rhs);
                let loc = match lhs.loc {
                    IRDAGMemLoc::Addr(addr) => IRDAGMemLoc::Addr(self.dag.new_int_binop(IRDAGNodeIntBinOpType::Add, &addr, &offset)),
                    IRDAGMemLoc::Named(named) => {
                        let addr_range = named.offset..(named.offset + self.lookup_local(&named.var_name).unwrap().size());
                        IRDAGMemLoc::NamedWithDynOffset(named, offset, addr_range)
                    }
                    IRDAGMemLoc::NamedWithDynOffset(named, dyn_offset, offset_range) =>
                        IRDAGMemLoc::NamedWithDynOffset(named, self.dag.new_int_binop(IRDAGNodeIntBinOpType::Add, &dyn_offset, &offset), offset_range)
                };
                self.last_temp_res = Some(IRDAGNodeTempResult::LVal(
                    IRDAGLVal { ty: elem_ty, loc: loc }
                ))
            }
            _ => {
                panic!("Unsupported binary operator {:?}", binary_operator_expression.operator.node);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
        // check whether this is a local, param, or global
        let loc_info = self.lookup_local(&identifier.name);
        if let Some(ty) = loc_info {
            self.last_temp_res = Some(IRDAGNodeTempResult::LVal(IRDAGLVal {
                ty: ty.clone(),
                loc: IRDAGMemLoc::Named(IRDAGNamedMemLoc { var_name: identifier.name.clone(), offset: 0 })
            }));
        } else if self.globals.func_decls.contains(&identifier.name) {
            assert!(self.last_func_ident.is_none());
            self.last_func_ident = Some(identifier.name.clone());
        } else {
            panic!("Unable to find identifier {}", identifier.name);
        }
    }

    fn visit_call_expression(&mut self, call_expression: &'ast CallExpression, span: &'ast Span) {
        self.visit_expression(&call_expression.callee.node, &call_expression.callee.span);
        let func_name = self.last_func_ident.take().unwrap();
        let args = Vec::from_iter(call_expression.arguments.iter().map(
            |arg_node| {
                self.visit_expression(&arg_node.node, &arg_node.span);
                self.last_temp_res_to_word().unwrap()
            }
        ));
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(self.dag.new_indom_call(&func_name, args)));
        self.new_block_reset();
    }

    fn visit_member_expression(
            &mut self,
            member_expression: &'ast lang_c::ast::MemberExpression,
            span: &'ast Span,
        ) {
        assert!(matches!(member_expression.operator.node, MemberOperator::Direct), "Only direct member operator (.) is supported");
        self.visit_expression(&member_expression.expression.node, &member_expression.expression.span);
        let lhs = self.last_temp_res.take().unwrap();
        let field_name = &member_expression.identifier.node.name;
        match &lhs {
            IRDAGNodeTempResult::LVal(lval) => {
                let field = match &lval.ty {
                    CaplanType::Struct(s) => s.find_field(field_name).cloned(),
                    CaplanType::StructRef(s_ref) => s_ref.borrow().find_field(field_name).cloned(),
                    _ => None
                }.expect("Cannot find field");
                self.last_temp_res = Some(
                    IRDAGNodeTempResult::LVal(
                        IRDAGLVal { ty: field.ty, loc: lval.loc.clone().apply_offset(field.offset as isize, &mut self.dag) }
                    )
                );
            }
            _ => panic!("Invalid member expression")
        }
    }

    fn visit_sizeofval(&mut self, sizeofval: &'ast lang_c::ast::SizeOfVal, span: &'ast Span) {
        self.visit_expression(&sizeofval.0.node, &sizeofval.0.span);
        let r = self.last_temp_res.take().unwrap();
        let size = match r {
            IRDAGNodeTempResult::LVal(lval) => lval.ty.size(),
            IRDAGNodeTempResult::Word(_) => 8
        };
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(self.dag.new_int_const(size as u64)));
    }

    fn visit_constant(&mut self, constant: &'ast lang_c::ast::Constant, span: &'ast Span) {
        match constant {
            Constant::Integer(integer) => {
                self.last_temp_res = Some(IRDAGNodeTempResult::Word(
                    self.dag.new_int_const(u64::from_str_radix(integer.number.as_ref(), 10).unwrap())
                ));
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

    fn visit_derived_declarator(
            &mut self,
            derived_declarator: &'ast lang_c::ast::DerivedDeclarator,
            span: &'ast Span,
        ) {
        self.decl_type.as_mut().map(|x| x.decorate_from_ast(derived_declarator));
    }

    fn visit_declarator(&mut self, declarator: &'ast lang_c::ast::Declarator, span: &'ast Span) {
        declarator.derived.iter().for_each(
            |derived| self.visit_derived_declarator(&derived.node, &derived.span)
        );
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
        let ty = self.decl_type.as_ref().unwrap().clone();
        self.locals.insert(name.clone(),ty.clone());
        for offset in (0..ty.size()).step_by(8) {
            self.local_named_locs.insert(IRDAGNamedMemLoc { var_name: name.clone(), offset: offset }, IRDAGNamedMemLocInfo::new());
        }
        if let Some(initializer_node) = init_declarator.initializer.as_ref() {
            self.visit_initializer(&initializer_node.node, &initializer_node.span);
            let rhs = self.last_temp_res.take().unwrap();
            let lhs = IRDAGNodeTempResult::LVal(
                IRDAGLVal { ty: ty, loc: IRDAGMemLoc::Named(IRDAGNamedMemLoc { var_name: name, offset: 0 }) }
            );
            self.gen_assign(lhs, rhs);
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