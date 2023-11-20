use std::collections::HashMap;

use crate::dag::*;
use crate::lang::{CaplanFunction, CaplanTranslationUnit};
use crate::utils::GCed;

// maintain the global codegen state
struct GlobalCodeGenContext {

}

impl GlobalCodeGenContext {
    fn new() -> Self {
        Self {}
    }
}

enum VarLocation {
    StackSlot(usize),
    GPR(usize),
    Nowhere
}

#[derive(Clone)]
enum GPRState {
    Taken(GCed<IRDAGNode>), // stores the result of node
    Reserved,
    Free
}

const GPR_N : usize = 32;
const GPR_IDX_X0 : usize = 0;
const GPR_IDX_RA : usize = 1;
const GPR_IDX_SP : usize = 2;
const GPR_RESERVED_LIST : [usize; 3] = [GPR_IDX_X0, GPR_IDX_RA, GPR_IDX_SP];

// this just generates code for a single function
struct FunctionCodeGen {
    // (current location, assigned stack slot, dirty)
    vars: HashMap<String, (VarLocation, usize, bool)>,
    stack_slots: Vec<String>,
    op_topo_stack: Vec<GCed<IRDAGNode>>,
    gpr_states: [GPRState; GPR_N],
}

impl FunctionCodeGen {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            stack_slots: Vec::new(),
            op_topo_stack: Vec::new(),
            gpr_states: std::array::from_fn(|_| GPRState::Free),
        }
    }

    fn generate_preamble(&mut self) {
        // TODO: implement
    }

    fn generate_node(&mut self, node: &IRDAGNode) {
        eprintln!("Codegen for node {:?}", node.cons);
    }

    // this consumes codegen itself as codegen can only happen once
    fn codegen(mut self, func: CaplanFunction, ctx: &mut GlobalCodeGenContext) {
        eprintln!("Codegen for function {}", func.name);

        // reserve some special registers
        for reserved_gpr in GPR_RESERVED_LIST {
            self.gpr_states[reserved_gpr] = GPRState::Reserved;
        }

        // TODO: let's not worry about arguments for now
        // for all variables that ever appear, allocate one stack slot
        for block in func.dag.blocks.iter() {
            for node in block.dag.iter() {
                let var_name_op: Option<String> = match &node.borrow().cons {
                    IRDAGNodeCons::Read(name) => Some(name.clone()),
                    IRDAGNodeCons::Write(name, _) => Some(name.clone()),
                    _ => None
                };
                if let Some(var_name_op) = var_name_op {
                    if !self.vars.contains_key(&var_name_op) {
                        // initially the variables are not stored anywhere
                        eprintln!("Allocated slot {} to variable {}", self.stack_slots.len(), var_name_op);
                        self.vars.insert(var_name_op.clone(), (VarLocation::Nowhere, self.stack_slots.len(), false));
                        self.stack_slots.push(var_name_op);
                    }
                }
            }
        }

        self.generate_preamble();

        // for each basic block, do a topo sort
        for (blk_id, block) in func.dag.blocks.iter().enumerate() {
            eprintln!("Codegen for basic block {}", blk_id);
            assert!(self.op_topo_stack.is_empty());
            for node in block.dag.iter() {
                let node_ref = node.borrow();
                if node_ref.dep_count == 0 && !node_ref.cons.is_control_flow() {
                    self.op_topo_stack.push(node.clone());
                }
            }
            while let Some(node) = self.op_topo_stack.pop() {
                let node_ref = node.borrow();
                self.generate_node(&*node_ref);
                for rev_dep in node_ref.rev_deps.iter() {
                    let mut rev_dep_m = rev_dep.borrow_mut();
                    rev_dep_m.dep_count -= 1;
                    if rev_dep_m.dep_count == 0 && !rev_dep_m.cons.is_control_flow() {
                        self.op_topo_stack.push(rev_dep.clone());
                    }
                }
            }
            if let Some(exit_node) = block.exit_node.as_ref() {
                self.generate_node(&*exit_node.borrow());
            }
        }

    }
}

pub struct CodeGen {
    translation_unit: CaplanTranslationUnit,
    ctx: GlobalCodeGenContext
}

impl CodeGen {
    pub fn new(translation_unit: CaplanTranslationUnit) -> Self {
        Self {
            translation_unit: translation_unit,
            ctx: GlobalCodeGenContext::new()
        }
    }

    pub fn codegen(mut self) {
        for func in self.translation_unit.functions {
            let func_codegen = FunctionCodeGen::new();
            func_codegen.codegen(func,&mut self.ctx);
        }
    }
}

