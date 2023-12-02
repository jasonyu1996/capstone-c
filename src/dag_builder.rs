use std::collections::HashMap;

use crate::lang::{CaplanParam, CaplanGlobalContext};
use crate::lang_defs::{CaplanType, try_modify_type_with_attr};
use crate::utils::{GCed, new_gced};
use crate::dag::{*, self};

use lang_c::ast::{UnaryOperator, UnaryOperatorExpression, ForInitializer, Label, MemberOperator, AsmStatement, IntegerBase, Attribute, Extension};
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
    type_attr_names: Vec<String>,
    break_targets: Vec<GCed<IRDAGNode>>,
    continue_targets: Vec<GCed<IRDAGNode>>,
    switch_target_info: Vec<SwitchTargetInfo<'ast>>,
    pub globals: &'ast CaplanGlobalContext,
    last_func_ident: Option<String>,
    id_counter: u64,
}

impl<'ast> IRDAGBuilder<'ast> {
    pub fn new(globals: &'ast CaplanGlobalContext) -> Self {
        let dag = IRDAG::new();
        let mut res = Self {
            dag: dag,
            locals: HashMap::new(),
            local_named_locs: HashMap::new(),
            last_temp_res: None,
            decl_type: None,
            decl_id_name: None,
            type_attr_names: Vec::new(),
            break_targets: Vec::new(),
            continue_targets: Vec::new(),
            switch_target_info: Vec::new(),
            globals: globals,
            last_func_ident: None,
            id_counter: 0
        };
        let init_label = res.new_label();
        res.place_label_node(&init_label);
        res
    }

    /* Low level operations (add nodes etc.) */

    pub fn add_dep(a: &GCed<IRDAGNode>, dep: &GCed<IRDAGNode>) {
        if a.borrow().id != dep.borrow().id {
            eprintln!("Depends: {} depends on {}", a.borrow().id, dep.borrow().id);
            (**dep).borrow_mut().add_to_rev_deps(a);
            (**a).borrow_mut().inc_dep_count();
        }
    }

    pub fn add_dep_mem_loc(a: &GCed<IRDAGNode>, dep: &IRDAGMemLoc) {
        match &dep {
            IRDAGMemLoc::Addr(addr, _, dyn_offset_op) => {
                Self::add_dep(&a, addr);
                if let Some(dyn_offset) = dyn_offset_op {
                    Self::add_dep(&a, dyn_offset);
                }
            }
            IRDAGMemLoc::Named(_) => (),
            IRDAGMemLoc::NamedWithDynOffset(_, dyn_offset, _) => Self::add_dep(&a, dyn_offset)
        }
    }

    fn add_nonlabel_node(&mut self, node: &GCed<IRDAGNode>) {
        let last_block = self.dag.blocks.last_mut().unwrap();
        assert!(last_block.exit_node.is_none()); // we should not have seen an exit node yet
        last_block.dag.push(node.clone());
        self.id_counter += 1;
        if node.borrow().cons.is_control_flow() {
            last_block.exit_node = Some(node.clone());
            self.new_block();
        }
    }

    /* Returns the index of the new basic block */
    fn new_block(&mut self) -> usize {
        self.dag.blocks.push(IRDAGBlock::new());
        self.dag.blocks.len() - 1
    }

    fn current_block_id(&self) -> usize {
        assert!(!self.dag.blocks.is_empty());
        self.dag.blocks.len() - 1
    }

    pub fn place_label_node(&mut self, label_node: &GCed<IRDAGNode>) {
        let blk_id = 
            if self.dag.blocks.last().unwrap().dag.is_empty() {
                self.current_block_id()
            } else {
                self.new_block()
            };
        let last_block = self.dag.blocks.last_mut().unwrap();
        assert!(last_block.dag.is_empty()); // must be the first node to add
        last_block.labeled = true;
        let mut label_node_ref = label_node.borrow_mut();
        if let IRDAGNodeCons::Label(None) = label_node_ref.cons {
            label_node_ref.cons = IRDAGNodeCons::Label(Some(blk_id));
        } else {
            panic!("Label has been placed elsewhere!");
        }
    }

    pub fn new_int_const(&mut self, v: u64) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntConst(v),
            false
        ));
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_int_binop(&mut self, op_type: IRDAGNodeIntBinOpType, a: &GCed<IRDAGNode>, b: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            a.borrow().vtype.clone(),
            IRDAGNodeCons::IntBinOp(op_type, a.clone(), b.clone()),
            false
        ));
        Self::add_dep(&res, a);
        Self::add_dep(&res, b);
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_int_unop(&mut self, op_type: IRDAGNodeIntUnOpType, a: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Int,
            IRDAGNodeCons::IntUnOp(op_type, a.clone()),
            false
        ));
        Self::add_dep(&res, a);
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_read(&mut self, mem_loc: IRDAGMemLoc, ty: IRDAGNodeVType) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            ty,
            IRDAGNodeCons::Read(mem_loc.clone()),
            false
        ));
        Self::add_dep_mem_loc(&res, &mem_loc);
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_write(&mut self, mem_loc: IRDAGMemLoc, v: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            v.borrow().vtype.clone(),
            IRDAGNodeCons::Write(mem_loc.clone(), v.clone()),
            true
        ));
        Self::add_dep(&res, v);
        Self::add_dep_mem_loc(&res, &mem_loc);
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_label(&mut self) -> GCed<IRDAGNode> {
        // label nodes are not added to the list until they are placed
        new_gced(IRDAGNode::new(
            0, // not used
            IRDAGNodeVType::Label,
            IRDAGNodeCons::Label(None),
            true
        ))
    }

    pub fn new_branch(&mut self, target: &GCed<IRDAGNode>, cond: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Void,
            IRDAGNodeCons::Branch(target.clone(), cond.clone()),
            true
        ));
        Self::add_dep(&res, cond);
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_jump(&mut self, target: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Void,
            IRDAGNodeCons::Jump(target.clone()),
            true
        ));
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_switch(&mut self, val: &GCed<IRDAGNode>, targets: Vec<GCed<IRDAGNode>>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Void,
            IRDAGNodeCons::Switch(val.clone(), targets),
            true
        ));
        Self::add_dep(&res, val);
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_indom_return(&mut self, ret_val: Option<GCed<IRDAGNode>>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Void,
            IRDAGNodeCons::InDomReturn(ret_val.clone()),
            true
        ));
        if let Some(ret_val_node) = ret_val {
            Self::add_dep(&res, &ret_val_node);
        }
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_indom_call(&mut self, callee: &str, args: Vec<GCed<IRDAGNode>>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Int,
            IRDAGNodeCons::InDomCall(String::from(callee), args.clone()),
            true
        ));
        for arg in args.iter() {
            Self::add_dep(&res, arg);
        }
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_address_of(&mut self, named_mem_loc: IRDAGNamedMemLoc, mut ty: CaplanType) -> GCed<IRDAGNode> {
        self.globals.target_conf.make_pointer(&mut ty);
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::from_caplan_type(&ty).unwrap(),
            IRDAGNodeCons::AddressOf(named_mem_loc),
            false
        ));
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_asm(&mut self, asm: String, outputs: Vec<IRDAGAsmOutput>, inputs: Vec<IRDAGAsmInput>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::Void,
            IRDAGNodeCons::Nop,
            true
        ));
        for out in outputs.iter() {
            Self::add_dep_mem_loc(&res, &out.loc);
        }
        for inp in inputs.iter() {
            Self::add_dep(&res, &inp.value);
        }
        res.borrow_mut().cons = IRDAGNodeCons::Asm(asm, outputs, inputs);
        self.add_nonlabel_node(&res);
        res
    }

    fn new_cap_resize(&mut self, v: &GCed<IRDAGNode>, size: usize) -> GCed<IRDAGNode> {
        let mut node = IRDAGNode::new(
            self.id_counter,
            v.borrow().vtype.clone(),
            IRDAGNodeCons::CapResize(v.clone(), size),
            false
        );
        if v.borrow().vtype.is_linear() {
            node.add_destruct(v.borrow().id);
        }
        let res = new_gced(node);
        Self::add_dep(&res, &v);
        self.add_nonlabel_node(&res);
        res
    }

    /* high-level build operations */

    pub fn build(&mut self, ast: &'ast FunctionDefinition, span: &'ast Span, 
                params: &[CaplanParam]) {
        for param in params.iter() {
            self.locals.insert(param.name.clone(), param.ty.clone());
            param.ty.visit_offset(&mut |offset, _| {
                self.local_named_locs.insert(IRDAGNamedMemLoc { var_name: param.name.clone(), offset: offset }, IRDAGNamedMemLocInfo::new());
            }, 0, &self.globals.target_conf);
        }
        self.visit_function_definition(ast, span);
        // self.dag.pretty_print();
    }

    // consume the builder and get the dag
    pub fn into_dag(self) -> (IRDAG, HashMap<String, CaplanType>) {
        (self.dag, self.locals)
    }

    // look up local variable
    pub fn lookup_local<'a>(&'a mut self, v: &str) -> Option<&'a mut CaplanType> {
        self.locals.get_mut(v)
    }

    pub fn lookup_local_imm<'a>(&'a self, v: &str) -> Option<&'a CaplanType> {
        self.locals.get(v)
    }

    fn lookup_identifier_type<'a>(&'a self, v: &str) -> Option<&'a CaplanType> {
        self.lookup_local_imm(v).or_else(
            || {
                self.globals.global_vars_to_ids.get(v).map(|&idx| &self.globals.global_vars[idx])
            }
        )
    }

    fn push_loop_info(&mut self, break_target: &GCed<IRDAGNode>, continue_target: &GCed<IRDAGNode>) {
        self.break_targets.push(break_target.clone());
        self.continue_targets.push(continue_target.clone());
    }

    fn pop_loop_info(&mut self) {
        self.break_targets.pop().unwrap();
        self.continue_targets.pop().unwrap();
    }

    // ty: type of the value being read, not of the location
    // dep_as_write: treat this as a write when managing dependencies if this read result is linear and is involved in a transfer-type operation
    fn read_location(&mut self, loc: &IRDAGMemLoc, ty: IRDAGNodeVType, dep_as_write: bool) -> GCed<IRDAGNode> {
        // ty might be a capability
        match loc {
            IRDAGMemLoc::Addr(_, _, _) => self.new_read(loc.clone(), ty),
            IRDAGMemLoc::Named(named_mem_loc) => {
                let ty_size = ty.size();
                let read_node = self.new_read(loc.clone(), ty);
                if dep_as_write {
                    self.write_named_mem_loc(named_mem_loc, &read_node);
                    if ty_size == 16 {
                        self.write_named_mem_loc(&named_mem_loc.clone().with_offset(8), &read_node);
                    }
                } else {
                    self.read_named_mem_loc(named_mem_loc, &read_node);
                    if ty_size == 16 {
                        self.read_named_mem_loc(&named_mem_loc.clone().with_offset(8), &read_node);
                    }
                }
                read_node
            }
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, _, offset_range) => {
                let read_node = self.new_read(loc.clone(), ty);
                for offset in offset_range.clone().step_by(8) {
                    let covered_loc = named_mem_loc.clone().with_offset(offset); // TODO: very brute force
                    if dep_as_write {
                        self.write_named_mem_loc(&covered_loc, &read_node);
                    } else {
                        self.read_named_mem_loc(&covered_loc, &read_node);
                    }
                }
                read_node
            }
        }
    }

    fn get_address(&mut self, lval: IRDAGLVal) -> GCed<IRDAGNode> {
        // TODO: need to adjust the type
        match lval.loc {
            IRDAGMemLoc::Addr(addr, static_offset, dyn_offset_op) => {
                eprintln!("Get address type {:?} {:?}, ", lval.ty, addr.borrow().vtype);
                let mut res = addr;
                if let Some(dyn_offset) = dyn_offset_op {
                    res = self.new_int_binop(IRDAGNodeIntBinOpType::Add, &res, &dyn_offset);
                }
                if static_offset != 0 {
                    let const_offset = self.new_int_const(static_offset as u64);
                    res = self.new_int_binop(IRDAGNodeIntBinOpType::Add, &res, &const_offset);
                }
                if res.borrow().vtype.size() == 16 { // is capability
                    let size = lval.ty.size(&self.globals.target_conf);
                    res = self.new_cap_resize(&res, size);
                }
                res
            }
            IRDAGMemLoc::Named(named) => self.new_address_of(named, lval.ty),
            IRDAGMemLoc::NamedWithDynOffset(named, dyn_offset, offset_range) => {
                let base_addr = self.new_address_of(named, lval.ty);
                let addr = self.new_int_binop(IRDAGNodeIntBinOpType::Add, &base_addr, &dyn_offset);
                addr
            }
        }
    }

    fn write_location(&mut self, loc: &IRDAGMemLoc, val: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let size = val.borrow().vtype.size();
        match loc {
            IRDAGMemLoc::Addr(_, _, _) => self.new_write(loc.clone(), val),
            IRDAGMemLoc::Named(named_mem_loc) => {
                let write_node = self.new_write(loc.clone(), val);
                self.write_named_mem_loc(named_mem_loc, &write_node);
                if size == 16 {
                    self.write_named_mem_loc(&named_mem_loc.clone().with_offset(8), &write_node);
                }
                write_node
            }
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, _, offset_range) => {
                let write_node = self.new_write(loc.clone(), val);
                for offset in offset_range.clone().step_by(8) {
                    let covered_loc = named_mem_loc.clone().with_offset(offset);
                    self.write_named_mem_loc(&covered_loc, &write_node);
                }
                write_node
            }
        }
    }
    
    fn result_to_word(&mut self, res: &IRDAGNodeTempResult, to_be_transferred: bool) -> Option<GCed<IRDAGNode>> {
        match res {
            IRDAGNodeTempResult::Word(word) => Some(word.clone()),
            IRDAGNodeTempResult::LVal(lval) => {
                match &lval.ty {
                    CaplanType::Array(inner_type, _) => {
                        let elem_lval = IRDAGLVal { ty: (**inner_type).clone(), loc: lval.loc.clone() };
                        Some(self.get_address(elem_lval))
                    }
                    _ => {
                        let size = lval.ty.size(&self.globals.target_conf);
                        if size <= 16 {
                            // read the variable location
                            Some(self.read_location(&lval.loc, IRDAGNodeVType::from_caplan_type(&lval.ty).unwrap(),
                                to_be_transferred && lval.ty.is_linear()))
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }

    fn last_temp_res_to_word(&mut self, to_be_transferred: bool) -> Option<GCed<IRDAGNode>> {
        self.last_temp_res.take().and_then(|x| self.result_to_word(&x, to_be_transferred))
    }

    fn last_temp_res_to_indexable_lval(&mut self, to_be_transferred: bool) -> Option<IRDAGLVal> {
        self.last_temp_res.take().and_then(
            |r| {
                match r {
                    IRDAGNodeTempResult::LVal(lval) => {
                        let ty = lval.ty;
                        let loc = lval.loc;
                        match ty {
                            CaplanType::Array(inner_type, _) =>
                                Some(IRDAGLVal { ty: *inner_type, loc: loc }),
                            CaplanType::RawPtr(inner_type) =>
                                Some(IRDAGLVal { ty: *inner_type.clone(), loc: IRDAGMemLoc::Addr(self.read_location(&loc, IRDAGNodeVType::RawPtr(*inner_type), false), 0, None) }),
                            CaplanType::NonlinPtr(inner_type) =>
                                Some(IRDAGLVal { ty: *inner_type.clone(), loc: IRDAGMemLoc::Addr(self.read_location(&loc, IRDAGNodeVType::NonlinPtr(*inner_type), false), 0, None) }),
                            CaplanType::LinPtr(inner_type) =>
                                Some(IRDAGLVal { ty: *inner_type.clone(), loc: IRDAGMemLoc::Addr(self.read_location(&loc, IRDAGNodeVType::LinPtr(*inner_type), false), 0, None) }),
                            // TODO: add linear capability support
                            _ => None
                        }
                    }
                    IRDAGNodeTempResult::Word(word) => {
                        let vtype = word.borrow().vtype.clone();
                        match vtype {
                            IRDAGNodeVType::RawPtr(inner_type) =>
                                Some(IRDAGLVal { ty: inner_type, loc: IRDAGMemLoc::Addr(word, 0, None) }),
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
        let l = self.last_temp_res_to_word(false).unwrap();
        self.visit_expression(&expr.rhs.node,
            &expr.rhs.span);
        let r = self.last_temp_res_to_word(false).unwrap();
        let l_ref = l.borrow();
        let r_ref = r.borrow();
        let res_word = 
            if let (IRDAGNodeCons::IntConst(l_const), IRDAGNodeCons::IntConst(r_const)) = (&l_ref.cons, &r_ref.cons) {
                self.new_int_const(dag::static_bin_op(op_type, *l_const, *r_const))
            } else {
                drop(l_ref);
                drop(r_ref); // somehow Rust is stupid
                self.new_int_binop(op_type, &l, &r)
            };
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(res_word));
    }

    // integer unary operator expression
    fn process_int_un_expr(&mut self, op_type: IRDAGNodeIntUnOpType, expr: &'ast UnaryOperatorExpression) {
        self.visit_expression(&expr.operand.node, &expr.operand.span);
        let r = self.last_temp_res_to_word(false).unwrap();
        let res_word = match &r.borrow().cons {
            IRDAGNodeCons::IntConst(r_const) => self.new_int_const(dag::static_un_op(op_type, *r_const)),
            _ => self.new_int_unop(op_type, &r)
        };
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(res_word));
    }

    fn write_named_mem_loc(&mut self, mem_loc: &IRDAGNamedMemLoc, node: &GCed<IRDAGNode>) {
        if let Some(v) = self.local_named_locs.get_mut(mem_loc) {
            if let Some(last_access) = v.last_access.as_ref() {
                // this current access must wait until after the previous access completes
                Self::add_dep(node, last_access);
            }
            v.last_access = Some(node.clone());
            v.last_write = Some(node.clone());
        }
    }

    fn read_named_mem_loc(&mut self, mem_loc: &IRDAGNamedMemLoc, node: &GCed<IRDAGNode>) {
        // eprintln!("{:?}", mem_loc);
        if let Some(v) = self.local_named_locs.get_mut(mem_loc) {
            if let Some(last_write) = v.last_write.as_ref() {
                Self::add_dep(node, last_write);
            }
            v.last_access = Some(node.clone());
        }
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
            let size = lval.ty.size(&self.globals.target_conf);
            if size > self.globals.target_conf.register_width {
                match rhs {
                    IRDAGNodeTempResult::Word(_) => panic!("Size mismatch for assignment"),
                    IRDAGNodeTempResult::LVal(rhs_lval) => {
                        if size != rhs_lval.ty.size(&self.globals.target_conf) {
                            panic!("Size mismatch for assignment");
                        } else {
                            let lhs_loc_base = lval.loc;
                            let rhs_loc_base = rhs_lval.loc;
                            let offset_collection = lval.ty.collect_offsets(&self.globals.target_conf);
                            for (elem_offset, elem_size) in offset_collection {
                                let lhs_loc = lhs_loc_base.clone().apply_offset(elem_offset as isize, self);
                                let rhs_loc = rhs_loc_base.clone().apply_offset(elem_offset as isize, self);
                                let read_res = self.read_location(&rhs_loc, IRDAGNodeVType::Int, false); // FIXME: this type should be looked up in type def
                                self.write_location(&lhs_loc, &read_res);
                            }
                        }
                    }
                }
            } else {
                let rhs_word = self.result_to_word(&rhs, false).expect("Size mismatch for assignment");
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
        let cond = self.last_temp_res_to_word(false).unwrap();
        let label_then = self.new_label();
        let label_taken = self.new_label();
        let _ = self.new_branch(&label_taken, &cond);
        self.new_block_reset();
        let _ = self.new_jump(&label_then);
        self.new_block_reset();
        self.place_label_node(&label_taken);
        self.visit_statement(&if_statement.then_statement.node, &if_statement.then_statement.span);
        if let Some(else_statement_node) = if_statement.else_statement.as_ref() {
            let label_end = self.new_label();
            let _ = self.new_jump(&label_end);
            self.new_block_reset();
            self.place_label_node(&label_then);
            self.visit_statement(&else_statement_node.node, &else_statement_node.span);
            self.new_block_reset();
            self.place_label_node(&label_end);
        } else {
            self.new_block_reset();
            self.place_label_node(&label_then);
        }
    }

    fn visit_while_statement(&mut self, while_statement: &'ast lang_c::ast::WhileStatement, span: &'ast Span) {
        let label_start = self.new_label();
        let label_taken = self.new_label();
        let label_end = self.new_label();
        self.push_loop_info(&label_end, &label_start);
        self.new_block_reset();
        self.place_label_node(&label_start);
        self.visit_expression(&while_statement.expression.node, &while_statement.expression.span);
        let cond = self.last_temp_res_to_word(false).unwrap();
        let _ = self.new_branch(&label_taken, &cond);
        self.new_block_reset();
        let _ = self.new_jump(&label_end);
        self.new_block_reset();
        self.place_label_node(&label_taken);
        self.visit_statement(&while_statement.statement.node, &while_statement.statement.span);
        let _ = self.new_jump(&label_start);
        self.new_block_reset();
        self.place_label_node(&label_end);
        self.pop_loop_info();
    }

    fn visit_static_assert(&mut self, static_assert: &'ast lang_c::ast::StaticAssert, span: &'ast Span) {
        panic!("Static assertion not supported");
    }

    fn visit_for_statement(&mut self, for_statement: &'ast lang_c::ast::ForStatement, span: &'ast Span) {
        let label_start = self.new_label();
        let label_end = self.new_label();
        let label_cont = self.new_label();

        self.push_loop_info(&label_end, &label_cont);

        self.visit_for_initializer(&for_statement.initializer.node, &for_statement.initializer.span);
        self.new_block_reset();
        self.place_label_node(&label_start);
        if let Some(cond_node) = for_statement.condition.as_ref() {
            self.visit_expression(&cond_node.node, &cond_node.span);
            let label_taken = self.new_label();
            let cond = self.last_temp_res_to_word(false).unwrap();
            self.new_branch(&label_taken, &cond);
            self.new_block_reset();
            self.new_jump(&label_end);
            self.new_block_reset();
            self.place_label_node(&label_taken);
        }

        self.visit_statement(&for_statement.statement.node, &for_statement.statement.span);
        self.new_block_reset();
        self.place_label_node(&label_cont);
        if let Some(step_node) = for_statement.step.as_ref() {
            self.visit_expression(&step_node.node, &step_node.span);
        }
        self.new_jump(&label_start);
        self.new_block_reset();
        self.place_label_node(&label_end);
        
        self.pop_loop_info();
    }

    fn visit_do_while_statement(
            &mut self,
            do_while_statement: &'ast lang_c::ast::DoWhileStatement,
            span: &'ast Span,
        ) {
        let label_start = self.new_label();
        let label_cont = self.new_label();
        let label_end = self.new_label();

        self.push_loop_info(&label_end, &label_cont);

        self.new_block_reset();
        self.place_label_node(&label_start);
        self.visit_statement(&do_while_statement.statement.node, &do_while_statement.statement.span);
        self.new_block_reset();
        self.place_label_node(&label_cont);
        self.visit_expression(&do_while_statement.expression.node, &do_while_statement.expression.span);
        let cond = self.last_temp_res_to_word(false).unwrap();
        self.new_branch(&label_start, &cond);
        self.new_block_reset();
        self.place_label_node(&label_end);

        self.pop_loop_info();
    }

    // TODO: use switch node type instead and delay this to codegen
    fn visit_switch_statement(
            &mut self,
            switch_statement: &'ast lang_c::ast::SwitchStatement,
            span: &'ast Span,
        ) {
        // TODO: because the current basic block organisation is stupid, we do weird stuff here
        let label_skip_expr = self.new_label();
        let label_skip_stmt = self.new_label();

        self.switch_target_info.push(SwitchTargetInfo::new());
        self.break_targets.push(label_skip_expr.clone());

        self.new_jump(&label_skip_stmt);
        self.new_block_reset();
        self.visit_statement(&switch_statement.statement.node, &switch_statement.statement.span);
        self.new_jump(&label_skip_expr);
        self.new_block_reset();
        self.place_label_node(&label_skip_stmt);

        self.break_targets.pop().unwrap();
        let switch_targets = self.switch_target_info.pop().unwrap();
        self.visit_expression(&switch_statement.expression.node, &switch_statement.expression.span);
        let val = self.last_temp_res_to_word(false).unwrap();

        for (expr, expr_span, target) in switch_targets.targets.iter() {
            self.visit_expression(expr, expr_span);
            let b_res = self.last_temp_res_to_word(false).unwrap();
            let cmp_res = self.new_int_binop(IRDAGNodeIntBinOpType::Eq, 
                &val, &b_res);
            self.new_branch(target, &cmp_res);
            self.new_block_reset();
        }

        if let Some(default_target) = switch_targets.default_target.as_ref() {
            self.new_jump(default_target);
            self.new_block_reset();
        }

        self.new_block_reset();
        self.place_label_node(&label_skip_expr);
    }

    fn visit_label(&mut self, label: &'ast lang_c::ast::Label, span: &'ast Span) {
        match label {
            Label::Case(expr) => {
                let label = self.new_label();
                self.new_block_reset();
                self.place_label_node(&label);

                self.switch_target_info.last_mut().unwrap().targets.push((&expr.node, &expr.span, label));
            }
            Label::Default => {
                let label = self.new_label();
                self.new_block_reset();
                self.place_label_node(&label);

                let last_switch_target_info = self.switch_target_info.last_mut().unwrap();
                assert!(last_switch_target_info.default_target.is_none(), "Duplicate default");
                last_switch_target_info.default_target = Some(label);
            }
            _ => {
                panic!("Unsupported label type {:?}", label);
            }
        }
    }

    fn visit_asm_statement(&mut self, asm_statement: &'ast lang_c::ast::AsmStatement, span: &'ast Span) {
        match asm_statement {
            AsmStatement::GnuBasic(basic_asm) => {
                let asm = basic_asm.node.concat().replace("\"", "");
                // place it in a separate basic block to maintain order (TODO: we might not want to do this)
                self.new_block_reset();
                self.new_asm(asm, Vec::new(), Vec::new());
                self.new_block_reset();
            }
            AsmStatement::GnuExtended(extended_asm_stmt) => {
                let asm_template = extended_asm_stmt.template.node.concat().replace("\"", "");
                let outputs = Vec::from_iter(extended_asm_stmt.outputs.iter().map(
                    |out_op| {
                        // parse constraint string
                        self.visit_expression(&out_op.node.variable_name.node, &out_op.node.variable_name.span);
                        let (out_loc, size) = match self.last_temp_res.take().unwrap() {
                            IRDAGNodeTempResult::LVal(lval) =>
                                (lval.loc, lval.ty.size(&self.globals.target_conf)),
                            IRDAGNodeTempResult::Word(_) => panic!("Output of asm needs to be lval")
                        };
                        assert!(size <= self.globals.target_conf.register_width, "Asm output lval cannot be wider than register");
                        let out_constraint = if out_op.node.constraints.node.concat().contains("+") {
                            IRDAGAsmOutputConstraint::ReadWrite
                        } else {
                            IRDAGAsmOutputConstraint::Overwrite
                        };
                        IRDAGAsmOutput {
                            symb_name: out_op.node.symbolic_name.as_ref().map(|n| n.node.name.clone()),
                            constraint: out_constraint,
                            loc: out_loc,
                            size: size
                        }
                    }
                ));
                let inputs = Vec::from_iter(extended_asm_stmt.inputs.iter().map(
                    |inp_op| {
                        self.visit_expression(&inp_op.node.variable_name.node, &inp_op.node.variable_name.span);
                        let inp_val = self.last_temp_res_to_word(true).unwrap();
                        IRDAGAsmInput {
                            symb_name: inp_op.node.symbolic_name.as_ref().map(|n| n.node.name.clone()),
                            value: inp_val
                        }
                    }
                ));
                // since asm is placed in a new block, we don't need to worry about read-write re-ordering
                self.new_asm(asm_template, outputs, inputs);
                self.new_block_reset();
            }
        }
    }

    fn visit_statement(&mut self, statement: &'ast Statement, span: &'ast Span) {
        match statement {
            Statement::Continue => {
                self.new_jump(&self.continue_targets.last().unwrap().clone());
                self.new_block_reset();
            }
            Statement::Break => {
                self.new_jump(&self.break_targets.last().unwrap().clone());
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
                    self.last_temp_res_to_word(true).unwrap() // TODO: only 8 byte can be returned
                });
                self.new_indom_return(ret_val_node);
                self.new_block_reset();
            }
            Statement::Compound(compound) =>
                compound.iter().for_each(|block_item_node| self.visit_block_item(&block_item_node.node, &block_item_node.span)),
            Statement::Labeled(labeled_stmt) => self.visit_labeled_statement(&labeled_stmt.node, &labeled_stmt.span),
            Statement::Switch(switch_stmt) => self.visit_switch_statement(&switch_stmt.node, &switch_stmt.span),
            Statement::Asm(asm_stmt) => self.visit_asm_statement(&asm_stmt.node, &asm_stmt.span),
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
                    Some(IRDAGNodeTempResult::Word(self.get_address(lval)))
                } else {
                    panic!("Cannot take address of integers");
                };
            }
            UnaryOperator::Indirection => {
                // dereference a pointer
                // TODO: some basic optimisation
                self.visit_expression(&unary_operator_expression.operand.node, &unary_operator_expression.operand.span);
                let r = self.last_temp_res_to_word(false).unwrap();
                let new_type = r.borrow().vtype.inner_type().expect("Invalid type for indirection access").clone();
                self.last_temp_res = Some(IRDAGNodeTempResult::LVal(IRDAGLVal {
                    ty: new_type,
                    loc: IRDAGMemLoc::Addr(r, 0, None)
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
            BinaryOperator::ShiftLeft => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Shl, binary_operator_expression);
            }
            BinaryOperator::ShiftRight => {
                self.process_int_bin_expr(IRDAGNodeIntBinOpType::Shr, binary_operator_expression);
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
                let lhs = self.last_temp_res_to_indexable_lval(false).unwrap();
                self.visit_expression(&binary_operator_expression.rhs.node, &binary_operator_expression.rhs.span);
                let rhs = self.last_temp_res_to_word(false).unwrap();
                // take address
                let elem_ty = lhs.ty.clone();
                // simple optimisation: check whether the index is statically known
                let rhs_ref = rhs.borrow();
                if let IRDAGNodeCons::IntConst(int_const) = &rhs_ref.cons {
                    let offset = elem_ty.size(&self.globals.target_conf) as u64 * *int_const;
                    let loc = lhs.loc.apply_offset(offset as isize, self);
                    self.last_temp_res = Some(IRDAGNodeTempResult::LVal(
                        IRDAGLVal { ty: elem_ty, loc: loc }
                    ));
                } else {
                    drop(rhs_ref);
                    let elem_size_const = self.new_int_const(elem_ty.size(&self.globals.target_conf) as u64);
                    let offset = self.new_int_binop(
                        IRDAGNodeIntBinOpType::Mul,
                        &elem_size_const, &rhs);
                    let loc = lhs.loc.clone().apply_dyn_offset(&offset, self);
                    self.last_temp_res = Some(IRDAGNodeTempResult::LVal(
                        IRDAGLVal { ty: elem_ty, loc: loc }
                    ));
                }
            }
            _ => {
                panic!("Unsupported binary operator {:?}", binary_operator_expression.operator.node);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
        // check whether this is a local, param, or global
        let loc_info = self.lookup_local_imm(&identifier.name).or_else(
            || self.globals.global_vars_to_ids.get(&identifier.name).map(|&idx| &self.globals.global_vars[idx])
        );
        // local or global variable
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
                self.last_temp_res_to_word(true).unwrap()
            }
        ));
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(self.new_indom_call(&func_name, args)));
        self.new_block_reset();
    }

    fn visit_member_expression(
            &mut self,
            member_expression: &'ast lang_c::ast::MemberExpression,
            span: &'ast Span,
        ) {
        self.visit_expression(&member_expression.expression.node, &member_expression.expression.span);

        let (ty, loc) = match &member_expression.operator.node {
            MemberOperator::Indirect => {
                let addr = self.last_temp_res_to_word(false).unwrap();
                let ty = addr.borrow().vtype.inner_type().unwrap().clone();
                (ty, IRDAGMemLoc::Addr(addr.clone(), 0, None))
            }
            MemberOperator::Direct => {
                let lhs = self.last_temp_res.take().unwrap();
                match lhs {
                    IRDAGNodeTempResult::LVal(lval) => (lval.ty, lval.loc),
                    _ => panic!("Invalid member expression")
                }
            }
        };

        let field_name = &member_expression.identifier.node.name;
        let field = match ty {
            CaplanType::Struct(s) => s.find_field(field_name).cloned(),
            CaplanType::StructRef(s_ref) => s_ref.borrow().find_field(field_name).cloned(),
            _ => None
        }.expect("Cannot find field");
        self.last_temp_res = Some(
            IRDAGNodeTempResult::LVal(
                IRDAGLVal { ty: field.ty, loc: loc.apply_offset(field.offset as isize, self) }
            )
        );
    }

    fn visit_sizeofval(&mut self, sizeofval: &'ast lang_c::ast::SizeOfVal, span: &'ast Span) {
        self.visit_expression(&sizeofval.0.node, &sizeofval.0.span);
        let r = self.last_temp_res.take().unwrap();
        let size = match r {
            IRDAGNodeTempResult::LVal(lval) => lval.ty.size(&self.globals.target_conf),
            IRDAGNodeTempResult::Word(wd) => wd.borrow().vtype.size()
        };
        self.last_temp_res = Some(IRDAGNodeTempResult::Word(self.new_int_const(size as u64)));
    }

    fn visit_constant(&mut self, constant: &'ast lang_c::ast::Constant, span: &'ast Span) {
        match constant {
            Constant::Integer(integer) => {
                let radix = match &integer.base {
                    IntegerBase::Binary => 2,
                    IntegerBase::Octal => 8,
                    IntegerBase::Decimal => 10,
                    IntegerBase::Hexadecimal => 16
                };
                self.last_temp_res = Some(IRDAGNodeTempResult::Word(
                    self.new_int_const(u64::from_str_radix(integer.number.as_ref(), radix).unwrap())
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
            DeclarationSpecifier::Extension(extension) => {
                for ext in extension.iter() {
                    match &ext.node {
                        Extension::Attribute(attr) => {
                            self.type_attr_names.push(attr.name.node.clone());
                        }
                        _ => ()
                    }
                }
            }
            _ => ()
        }
    }

    fn visit_derived_declarator(
            &mut self,
            derived_declarator: &'ast lang_c::ast::DerivedDeclarator,
            span: &'ast Span,
        ) {
        self.decl_type.as_mut().map(|x| x.decorate_from_ast(derived_declarator, &self.globals.target_conf));
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
        let mut ty = self.decl_type.as_ref().unwrap().clone();
        // call the type modifiers defined by the type attributes        
        for attr_name in self.type_attr_names.iter() {
            assert!(try_modify_type_with_attr(&mut ty, attr_name), "Failed to apply the type attribute {}", attr_name);
        }
        eprintln!("Declared new local variable {} with type {:?}", name, ty);
        self.locals.insert(name.clone(),ty.clone());
        let offset_collection = ty.collect_offsets(&self.globals.target_conf);
        for (offset, _) in offset_collection {
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
        assert!(self.type_attr_names.is_empty());
        assert!(self.decl_type.is_none()); // not already in a declaration
        for decl_specifier in declaration.specifiers.iter() {
            self.visit_declaration_specifier(&decl_specifier.node, &decl_specifier.span);
        }
        assert!(self.decl_type.is_some()); // we should have got a type
        for init_decl in declaration.declarators.iter() {
            self.visit_init_declarator(&init_decl.node, &init_decl.span);
        }
        self.decl_type = None;
        self.type_attr_names.clear();
    }

    fn visit_function_definition(
            &mut self,
            function_definition: &'ast FunctionDefinition,
            span: &'ast Span,
        ) {
        self.visit_statement(&function_definition.statement.node, &function_definition.statement.span);
    }

}