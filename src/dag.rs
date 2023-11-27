use crate::{utils::{GCed, new_gced}, lang_defs::CaplanType, target_conf::CaplanTargetConf, dag_builder::IRDAGBuilder};

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
    Label
}

impl IRDAGNodeVType {
    pub fn size(&self) -> usize {
        match self {
            IRDAGNodeVType::Void => 8,
            IRDAGNodeVType::Int => 8,
            IRDAGNodeVType::Dom => 16,
            IRDAGNodeVType::RawPtr(_) => 8,
            IRDAGNodeVType::LinPtr(_) => 16,
            IRDAGNodeVType::NonlinPtr(_) => 16,
            IRDAGNodeVType::Label => 8 // TODO: labels don't really have a size
        }
    }

    // returns if the type is linear (i.e., it cannot be duplicated)
    pub fn is_linear(&self) -> bool {
        match self {
            IRDAGNodeVType::LinPtr(_) => true,
            IRDAGNodeVType::Dom => true,
            _ => false
        }
    }

    pub fn from_caplan_type(caplan_type: &CaplanType) -> Option<Self> {
        match caplan_type {
            CaplanType::Void => Some(IRDAGNodeVType::Void),
            CaplanType::Dom => Some(IRDAGNodeVType::Dom),
            CaplanType::Int => Some(IRDAGNodeVType::Int),
            CaplanType::RawPtr(inner_type) => Some(IRDAGNodeVType::RawPtr(*inner_type.clone())),
            CaplanType::LinPtr(inner_type) => Some(IRDAGNodeVType::LinPtr(*inner_type.clone())),
            CaplanType::NonlinPtr(inner_type) => Some(IRDAGNodeVType::NonlinPtr(*inner_type.clone())),
            _ => None
        }
    }
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
    pub fn apply_offset(self, offset: isize, dag_builder: &mut IRDAGBuilder) -> Self {
        match self {
            IRDAGMemLoc::Addr(addr) => {
                let const_node = dag_builder.new_int_const(offset as u64);
                IRDAGMemLoc::Addr(dag_builder.new_int_binop(IRDAGNodeIntBinOpType::Add,
                    &addr, &const_node))
            }
            IRDAGMemLoc::Named(named_mem_loc) =>
                IRDAGMemLoc::Named(named_mem_loc.apply_offset(offset)),
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, dyn_offset, offset_range) =>
                IRDAGMemLoc::NamedWithDynOffset(named_mem_loc.apply_offset(offset), dyn_offset, offset_range)
        }
    }

    // type of the value stored at the memory location
    // pub fn get_type(&self) -> &CaplanType {
        // match self {
        //     IRDAGMemLoc::Named(mem_loc)
        // }
    // }
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
    AddressOf(IRDAGNamedMemLoc),
    // inline assembly
    Asm(String)
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
            Self::AddressOf(_) => write!(f, "AddressOf"),
            Self::Asm(arg0) => f.debug_tuple("Asm").field(arg0).finish()
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
    pub fn new() -> Self {
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
}

impl IRDAG {
    pub fn new() -> Self {
        IRDAG {
            blocks: vec![IRDAGBlock::new()]
        }
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

/* Static operations */

pub fn static_bin_op(op: IRDAGNodeIntBinOpType, a: u64, b: u64) -> u64 {
    match op {
        IRDAGNodeIntBinOpType::Add => a + b,
        IRDAGNodeIntBinOpType::Sub => a - b,
        IRDAGNodeIntBinOpType::Mul => a * b,
        IRDAGNodeIntBinOpType::Div => a / b,
        IRDAGNodeIntBinOpType::And => a & b,
        IRDAGNodeIntBinOpType::Or => a | b,
        IRDAGNodeIntBinOpType::Xor => a ^ b,
        IRDAGNodeIntBinOpType::Eq => if a == b { 1 } else { 0 },
        IRDAGNodeIntBinOpType::NEq => if a == b { 0 } else { 1 },
        IRDAGNodeIntBinOpType::GreaterThan => if a > b { 1 } else { 0 },
        IRDAGNodeIntBinOpType::GreaterEq => if a >= b { 1 } else { 0 },
        IRDAGNodeIntBinOpType::LessThan => if a < b { 1 } else { 0 },
        IRDAGNodeIntBinOpType::LessEq => if a <= b { 1 } else { 0 },
    }
}

pub fn static_un_op(op: IRDAGNodeIntUnOpType, v: u64) -> u64 {
    match op {
        IRDAGNodeIntUnOpType::Not => !v,
        IRDAGNodeIntUnOpType::Negate => if v == 0 { 1 } else { 0 },
        IRDAGNodeIntUnOpType::Neg => v.wrapping_neg()
    }
}
