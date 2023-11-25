use crate::{utils::{GCed, new_gced}, lang_defs::CaplanType};

pub type IRDAGNodeId = u64;

// here the values of all these types can be held in 8/16 bytes
#[derive(Clone, Debug)]
pub enum IRDAGNodeVType {
    Void,
    Int,
    Dom,
    RawPtr(CaplanType), // TODO: probably we don't need this info here
    LinPtr(CaplanType),
    NonlinPtr(CaplanType),
    Label,
    Func
}

#[derive(Debug)]
pub struct IRDAGNode {
    pub id: IRDAGNodeId, // unique within basic block
    pub vtype: IRDAGNodeVType,
    pub cons: IRDAGNodeCons,
    // does evaluating this node have side effects?
    pub side_effects: bool,
    // reverse dependencies
    pub rev_deps: Vec<GCed<IRDAGNode>>,
    // number of dependencies
    pub dep_count: u64
}

#[derive(Copy, Clone, Debug)]
pub enum IRDAGNodeIntBinOpType {
    Add, Sub, Mul, Div, Or, Xor, And,
    Eq, LessThan, GreaterThan, LessEq, GreaterEq, NEq,
    // TODO: more
}


#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct IRDAGNamedMemLoc {
    pub var_name: String,
    pub offset: usize
}

impl IRDAGNamedMemLoc {
    pub fn apply_offset(self, offset: isize) -> Self {
        Self {
            var_name: self.var_name,
            offset: self.offset.checked_add_signed(offset).unwrap()
        }
    }

    pub fn with_offset(self, offset: usize) -> Self {
        Self {
            var_name: self.var_name,
            offset: offset
        }
    }
}

pub type OffsetRange = std::ops::Range<usize>;

#[derive(Debug, Clone)]
pub enum IRDAGMemLoc {
    Named(IRDAGNamedMemLoc),
    // static location + dynamic offset with possible range recorded
    NamedWithDynOffset(IRDAGNamedMemLoc, GCed<IRDAGNode>, OffsetRange),
    Addr(GCed<IRDAGNode>)
}

impl IRDAGMemLoc {
    // only for static offset
    pub fn apply_offset(self, offset: isize, dag: &mut IRDAG) -> Self {
        match self {
            IRDAGMemLoc::Addr(addr) => {
                let const_node = dag.new_int_const(offset as u64);
                IRDAGMemLoc::Addr(dag.new_int_binop(IRDAGNodeIntBinOpType::Add,
                    &addr, &const_node))
            }
            IRDAGMemLoc::Named(named_mem_loc) =>
                IRDAGMemLoc::Named(named_mem_loc.apply_offset(offset)),
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, dyn_offset, offset_range) =>
                IRDAGMemLoc::NamedWithDynOffset(named_mem_loc.apply_offset(offset), dyn_offset, offset_range)
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum IRDAGNodeIntUnOpType {
    Neg, Not, Negate 
}


#[derive(Clone)]
pub enum IRDAGNodeTempResult {
    LVal(IRDAGLVal),
    Word(GCed<IRDAGNode>) 
}

#[derive(Clone)]
pub struct IRDAGLVal {
    pub ty: CaplanType,
    pub loc: IRDAGMemLoc
}


pub enum IRDAGNodeCons {
    // simple operations on integers; output is also integer
    IntConst(u64),
    IntBinOp(IRDAGNodeIntBinOpType, GCed<IRDAGNode>, GCed<IRDAGNode>),
    IntUnOp(IRDAGNodeIntUnOpType, GCed<IRDAGNode>),
    IncOffset(GCed<IRDAGNode>, GCed<IRDAGNode>),
    // conditional branch
    Branch(GCed<IRDAGNode>, GCed<IRDAGNode>),
    // unconditional jump
    Jump(GCed<IRDAGNode>),
    // switch (val, jump targets)
    Switch(GCed<IRDAGNode>, Vec<GCed<IRDAGNode>>),
    // in-domain call
    InDomCall(String, Vec<GCed<IRDAGNode>>),
    // domain call
    DomCall(GCed<IRDAGNode>, Vec<GCed<IRDAGNode>>),
    // in-domain return
    InDomReturn(Option<GCed<IRDAGNode>>),
    // domain return, a capability needed
    DomReturn(GCed<IRDAGNode>),
    // local, parameter or global variable
    Read(IRDAGMemLoc),
    // write to local, parameter or global variable
    Write(IRDAGMemLoc, GCed<IRDAGNode>),
    // label of a specific basic block
    // Label(None) is a label that has not been placed
    Label(Option<usize>),
    // take the address of a symbol
    AddressOf(IRDAGNamedMemLoc)
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
            Self::Switch(_, _) => write!(f, "Switch"),
            Self::InDomCall(_, _) => write!(f, "InDomCall"),
            Self::DomCall(_, _) => write!(f, "DomCall"),
            Self::InDomReturn(_) => write!(f, "InDomReturn"),
            Self::DomReturn(_) => write!(f, "DomReturn"),
            Self::Read(_) => write!(f, "Read"),
            Self::Write(_, _) => write!(f, "Write"),
            Self::Label(arg0) => f.debug_tuple("Label").field(arg0).finish(),
            Self::AddressOf(_) => write!(f, "AddressOf")
        }
    }
}

impl IRDAGNodeCons {
    pub fn is_control_flow(&self) -> bool {
        match self {
            IRDAGNodeCons::Branch(_, _) => true,
            IRDAGNodeCons::Jump(_) => true,
            IRDAGNodeCons::Switch(_, _) => true,
            IRDAGNodeCons::InDomCall(_, _) => true,
            IRDAGNodeCons::DomCall(_, _) => true,
            IRDAGNodeCons::InDomReturn(_) => true,
            IRDAGNodeCons::DomReturn(_) => true,
            _ => {
                false
            }
        }
    }
}

impl IRDAGNode {
    pub fn new(id: u64, vtype: IRDAGNodeVType, cons: IRDAGNodeCons, side_effects: bool) -> Self {
        Self {
            id: id,
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

pub struct IRDAGBlock {
    pub dag: Vec<GCed<IRDAGNode>>,
    pub labeled: bool,
    pub exit_node: Option<GCed<IRDAGNode>> /* the control flow node that jumps to another basic block */
}

impl IRDAGBlock {
    fn new() -> Self {
        Self {
            dag: Vec::new(),
            labeled: false,
            exit_node: None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.dag.is_empty() && !self.labeled && self.exit_node.is_none()
    }
}

pub struct IRDAG {
    pub blocks: Vec<IRDAGBlock>, // DAGs are separate per basic block
    id_counter: u64
}

impl IRDAG {
    pub fn new() -> Self {
        IRDAG {
            blocks: vec![IRDAGBlock::new()],
            id_counter: 0
        }
    }

    pub fn add_dep(a: &GCed<IRDAGNode>, dep: &GCed<IRDAGNode>) {
        eprintln!("Depends: {} depends on {}", a.borrow().id, dep.borrow().id);
        (**dep).borrow_mut().add_to_rev_deps(a);
        (**a).borrow_mut().inc_dep_count();
    }

    fn add_nonlabel_node(&mut self, node: &GCed<IRDAGNode>) {
        let last_block = self.blocks.last_mut().unwrap();
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
        self.blocks.push(IRDAGBlock::new());
        self.blocks.len() - 1
    }

    fn current_block_id(&self) -> usize {
        assert!(!self.blocks.is_empty());
        self.blocks.len() - 1
    }

    pub fn place_label_node(&mut self, label_node: &GCed<IRDAGNode>) {
        let blk_id = 
            if self.blocks.last().unwrap().dag.is_empty() {
                self.current_block_id()
            } else {
                self.new_block()
            };
        let last_block = self.blocks.last_mut().unwrap();
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
            IRDAGNodeVType::Int,
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

    pub fn new_read(&mut self, mem_loc: IRDAGMemLoc) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            // TODO: should be looked up, assuming int for now
            IRDAGNodeVType::Int,
            IRDAGNodeCons::Read(mem_loc.clone()),
            false
        ));
        match &mem_loc {
            IRDAGMemLoc::Addr(addr) => Self::add_dep(&res, addr),
            IRDAGMemLoc::Named(_) => (),
            IRDAGMemLoc::NamedWithDynOffset(_, dyn_offset, _) => Self::add_dep(&res, dyn_offset)
        }
        self.add_nonlabel_node(&res);
        res
    }

    pub fn new_write(&mut self, mem_loc: IRDAGMemLoc, v: &GCed<IRDAGNode>) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            v.borrow().vtype.clone(),
            IRDAGNodeCons::Write(mem_loc.clone(), v.clone()),
            false
        ));
        Self::add_dep(&res, v);
        match &mem_loc {
            IRDAGMemLoc::Addr(addr) => Self::add_dep(&res, addr),
            IRDAGMemLoc::Named(_) => (),
            IRDAGMemLoc::NamedWithDynOffset(_, dyn_offset, _) => Self::add_dep(&res, dyn_offset)
        }
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

    pub fn new_address_of(&mut self, named_mem_loc: IRDAGNamedMemLoc, ty: CaplanType) -> GCed<IRDAGNode> {
        let res = new_gced(IRDAGNode::new(
            self.id_counter,
            IRDAGNodeVType::RawPtr(ty),
            IRDAGNodeCons::AddressOf(named_mem_loc),
            false
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
                let node_label = format!("{} ({:?}: {:?})", node.borrow().id, node.borrow().cons, node.borrow().vtype).replace("\"", "\\\"");
                println!("\"{}\";", node_label);
                for rev_dep in node.borrow().rev_deps.iter() {
                    let rev_dep_label = format!("{} ({:?}: {:?})", rev_dep.borrow().id, rev_dep.borrow().cons, rev_dep.borrow().vtype).replace("\"", "\\\"");
                    println!("\"{}\" -> \"{}\";", node_label, rev_dep_label);
                }
            }
        }
        println!("}}");
    }
}
