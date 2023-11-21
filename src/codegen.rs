use core::panic;
use std::collections::HashMap;
use std::process::exit;

use crate::dag::*;
use crate::lang::{CaplanFunction, CaplanTranslationUnit};
use crate::utils::GCed;

mod code_printer;
mod arch_defs;

use arch_defs::*;

// maintain the global codegen state
struct GlobalCodeGenContext {

}

impl GlobalCodeGenContext {
    fn new() -> Self {
        Self {}
    }
}


enum VarLocation {
    StackSlot,
    GPR(RegId)
}

enum TempLocation {
    SpilledStackSlot(usize),
    GPR(RegId),
    Nowhere
}

#[derive(Clone, Copy, Debug)]
enum GPRState {
    Taken(IRDAGNodeId), // stores the result of node with id
    Pinned(IRDAGNodeId), // pin a reg if it is going to be used immediately
    Reserved,
    Free
}

#[derive(Clone, Copy)]
enum SpilledStackSlotState {
    Taken(IRDAGNodeId),
    Free
}

struct VarState {
    loc: VarLocation,
    stack_slot: usize,
    dirty: bool
}

struct TempState {
    var: Option<VarId>, // if this has the value of a variable
    loc: TempLocation,
    rev_deps_to_eval: usize
}

type VarId = usize;

// this just generates code for a single function
struct FunctionCodeGen {
    vars_to_ids: HashMap<String, VarId>,
    vars: Vec<VarState>,
    stack_slots: Vec<VarId>,
    temps: HashMap<IRDAGNodeId, TempState>,
    spilled_stack_slots: Vec<SpilledStackSlotState>,
    op_topo_stack: Vec<GCed<IRDAGNode>>,
    gpr_states: [GPRState; GPR_N],
}

impl FunctionCodeGen {
    fn new() -> Self {
        Self {
            vars_to_ids: HashMap::new(),
            vars: Vec::new(),
            stack_slots: Vec::new(),
            temps: HashMap::new(),
            spilled_stack_slots: Vec::new(),
            op_topo_stack: Vec::new(),
            gpr_states: [GPRState::Free; GPR_N],
        }
    }

    fn generate_prologue(&mut self) {
        // TODO: implement
    }

    fn spill_reg(&mut self, reg_id: RegId, node_id: IRDAGNodeId) {
        // find a spill slot
        let spill_slot = if let Some((spill_slot, slot_state)) = self.spilled_stack_slots.iter_mut().enumerate().find(
            |(_, slot_state)| {
                matches!(**slot_state, SpilledStackSlotState::Free)
            }
        ) {
            spill_slot
        } else {
            self.spilled_stack_slots.push(SpilledStackSlotState::Free);
            self.spilled_stack_slots.len() - 1
        };
        code_printer::print_store_to_stack_slot(reg_id, self.spill_slot_to_stack_slot(spill_slot));
        self.spilled_stack_slots[spill_slot] = SpilledStackSlotState::Taken(node_id);
        self.temps.get_mut(&node_id).unwrap().loc = TempLocation::SpilledStackSlot(spill_slot);
        self.gpr_states[reg_id] = GPRState::Free;
    }

    fn spill_any_reg(&mut self) -> RegId {
        let dead = self.gpr_states.iter().enumerate().find(
            |(_, s)| {
                match s {
                    GPRState::Taken(node_id) => {
                        self.temps.get(node_id).unwrap().rev_deps_to_eval == 0
                    }
                    GPRState::Pinned(node_id) => {
                        self.temps.get(node_id).unwrap().rev_deps_to_eval == 0 // FIXME: not done correctly
                    }
                    GPRState::Reserved => false,
                    GPRState::Free => {
                        panic!("Attempting to spill when there are still free GPRs");
                    }
                }
            }
        ).map(|(idx, _)| idx);
        if let Some(dead_reg) = dead {
            // no need to write back either
            dead_reg
        } else {
            let (reg_to_spill, &mut node_to_spill) = self.gpr_states.iter_mut().enumerate().find_map(|(idx, s)| {
                if let GPRState::Taken(node_id) = s {
                    Some((idx, node_id))
                } else {
                    None
                }
            }).unwrap();
            self.spill_reg(reg_to_spill, node_to_spill);
            reg_to_spill
        }
    }

    fn try_assign_reg_no_spill(&mut self, node_id: IRDAGNodeId) -> Option<RegId> {
        self.gpr_states.iter_mut().enumerate().find(|(_, s)| matches!(**s, GPRState::Free)).map(
            |(idx, s)| {
                *s = GPRState::Taken(node_id);
                self.temps.get_mut(&node_id).unwrap().loc = TempLocation::GPR(idx);
                idx
            }
        )
    }

    fn gen_func_label(&self, func_name: &str) -> String {
        String::from(func_name)
    }

    fn gen_func_block_label(&self, func_name: &str, blk_id: usize) -> String {
        format!("_{}.{}", func_name, blk_id)
    }
    
    fn assign_reg(&mut self, node_id: IRDAGNodeId) -> RegId {
        if let Some(reg_id) = self.try_assign_reg_no_spill(node_id) {
            reg_id
        } else {
            let reg_id = self.spill_any_reg();
            self.gpr_states[reg_id] = GPRState::Taken(node_id); // initially it is pinned to prevent immediate spilling
            self.temps.get_mut(&node_id).unwrap().loc = TempLocation::GPR(reg_id);
            reg_id
        }
    }

    fn spill_slot_to_stack_slot(&self, spill_slot: usize) -> usize {
        self.stack_slots.len() + spill_slot
    }

    fn pin_gpr(&mut self, reg_id: RegId) {
        match self.gpr_states[reg_id] {
            GPRState::Taken(node_id) => {
                self.gpr_states[reg_id] = GPRState::Pinned(node_id);
            }
            GPRState::Pinned(_) => (),
            _ => {
                panic!("Failed to pin GPR {} in state {:?}", reg_id, self.gpr_states[reg_id]);
            }
        }
    }

    fn unpin_gpr(&mut self, reg_id: RegId) {
        match self.gpr_states[reg_id] {
            GPRState::Taken(_) => (),
            GPRState::Pinned(node_id) => {
                self.gpr_states[reg_id] = GPRState::Taken(node_id);
            }
            _ => {
                panic!("Failed to pin GPR {} in state {:?}", reg_id, self.gpr_states[reg_id]);
            }
        }
    }

    fn prepare_source_reg(&mut self, source_node_id: IRDAGNodeId) -> RegId {
        eprintln!("Prepare source reg {}", source_node_id);
        let temp_state = self.temps.get(&source_node_id).unwrap();
        // bring it to register
        let reg_id = match temp_state.loc {
            TempLocation::GPR(reg_id) => reg_id,
            TempLocation::SpilledStackSlot(spill_slot) => {
                let r = self.assign_reg(source_node_id); // this already records the temp location in reg
                self.spilled_stack_slots[spill_slot] = SpilledStackSlotState::Free;
                code_printer::print_load_from_stack_slot(r, self.spill_slot_to_stack_slot(spill_slot));
                r
            }
            TempLocation::Nowhere => {
                panic!("Source temporary has not been evaluated.");
            }
        };
        self.pin_gpr(reg_id);
        self.temps.get_mut(&source_node_id).unwrap().rev_deps_to_eval -= 1;
        reg_id
    }

    fn generate_node(&mut self, func_name: &str, node: &IRDAGNode) {
        eprintln!("Codegen for node {} ({:?})", node.id, node.cons);
        match &node.cons {
            IRDAGNodeCons::IntConst(val) => {
                code_printer::print_li(self.assign_reg(node.id), *val);
            }
            IRDAGNodeCons::IntBinOp(op_type, s1, s2) => {
                let rs1 = self.prepare_source_reg(s1.borrow().id);
                let rs2 = self.prepare_source_reg(s2.borrow().id);
                self.unpin_gpr(rs1);
                self.unpin_gpr(rs2);
                let rd = self.assign_reg(node.id);
                match op_type {
                    IRDAGNodeIntBinOpType::Add => code_printer::print_add(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::Sub => code_printer::print_sub(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::Mul => code_printer::print_mul(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::Div => code_printer::print_div(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::And => code_printer::print_and(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::Or => code_printer::print_or(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::Xor => code_printer::print_xor(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::Eq => code_printer::print_eq(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::NEq => code_printer::print_neq(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::LessThan => code_printer::print_lt(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::GreaterThan => code_printer::print_lt(rd, rs2, rs1),
                    IRDAGNodeIntBinOpType::LessEq => code_printer::print_le(rd, rs1, rs2),
                    IRDAGNodeIntBinOpType::GreaterEq => code_printer::print_le(rd, rs1, rs1)
                }
            }
            IRDAGNodeCons::Write(var_name, source) => {
                let var_id = *self.vars_to_ids.get(var_name).unwrap();
                let rs = self.prepare_source_reg(source.borrow().id);
                self.unpin_gpr(rs);
                let rd = self.assign_reg(node.id);
                let var_state = self.vars.get_mut(var_id).unwrap();
                // look at current location of the variable
                if let VarLocation::GPR(old_reg_id) = var_state.loc {
                    // disassociate the old reg and node
                    if let GPRState::Taken(node_id) = self.gpr_states[old_reg_id] {
                        self.temps.get_mut(&node_id).unwrap().var = None;
                    }
                }
                
                var_state.loc = VarLocation::GPR(rd);
                var_state.dirty = true;
                self.temps.get_mut(&node.id).unwrap().var = Some(var_id);

                if rd != rs {
                    code_printer::print_mv(rd, rs);
                }
            }
            IRDAGNodeCons::Read(var_name) => {
                // see where it is right now
                let var_id = *self.vars_to_ids.get(var_name).unwrap();
                let var_state = self.vars.get(var_id).unwrap();

                // look at current location of the variable
                let _ = match var_state.loc {
                    VarLocation::GPR(reg_id) => {
                        // already in a register
                        // just grab the register
                        if let GPRState::Taken(node_id) = self.gpr_states[reg_id] {
                            self.temps.get_mut(&node_id).unwrap().var = None; // disassociate with old node
                        }
                        self.temps.get_mut(&node.id).unwrap().loc = TempLocation::GPR(reg_id);
                        self.gpr_states[reg_id] = GPRState::Taken(node.id);
                        reg_id
                    }
                    VarLocation::StackSlot => {
                        // need to load from stack
                        let stack_slot = var_state.stack_slot;
                        let reg_id = self.assign_reg(node.id);
                        let var_state_mut = self.vars.get_mut(var_id).unwrap();
                        var_state_mut.dirty = false; // just loaded, not dirty
                        var_state_mut.loc = VarLocation::GPR(reg_id);
                        code_printer::print_load_from_stack_slot(reg_id, stack_slot);
                        reg_id
                    }
                };
                self.temps.get_mut(&node.id).unwrap().var = Some(var_id);
            }
            IRDAGNodeCons::Jump(target) => {
                if let IRDAGNodeCons::Label(Some(blk_id)) = target.borrow().cons {
                    code_printer::print_jump_label(&self.gen_func_block_label(func_name, blk_id));
                } else {
                    panic!("Invalid jump operation");
                }
            }
            IRDAGNodeCons::Branch(target, cond) => {
                let rs = self.prepare_source_reg(cond.borrow().id);
                self.unpin_gpr(rs);
                if let IRDAGNodeCons::Label(Some(blk_id)) = target.borrow().cons {
                    code_printer::print_branch_label(&self.gen_func_block_label(func_name, blk_id), rs);
                } else {
                    panic!("Invalid branching operation");
                }
            }
            _ => {
                panic!("Unrecognised node {:?}", node.cons);
            }
        }
    }

    fn codegen_block_reset(&mut self, func_name: &str, block: &IRDAGBlock) {
        self.temps.clear();
        for node in block.dag.iter() {
            let node_ref = node.borrow();
            self.temps.insert(node_ref.id, TempState {
                var: None,
                loc: TempLocation::Nowhere,
                rev_deps_to_eval: node_ref.rev_deps.len()
            });
        }
        
        self.gpr_states.fill(GPRState::Free);
        // reserve some special registers
        for reserved_gpr in GPR_RESERVED_LIST {
            self.gpr_states[reserved_gpr] = GPRState::Reserved;
        }

        for var_state in self.vars.iter_mut() {
            var_state.dirty = false;
            var_state.loc = VarLocation::StackSlot;
        }

        for spill_slot_state in self.spilled_stack_slots.iter_mut() {
            *spill_slot_state = SpilledStackSlotState::Free;
        }
    }

    fn codegen_block_end_cleanup(&mut self, func_name: &str, block: &IRDAGBlock) {
        // write back all dirty variables
        for var_state in self.vars.iter_mut() {
            if var_state.dirty {
                match var_state.loc {
                    VarLocation::GPR(reg_id) => {
                        code_printer::print_store_to_stack_slot(reg_id, var_state.stack_slot);
                        var_state.loc = VarLocation::StackSlot;
                        if let GPRState::Taken(node_id) = self.gpr_states[reg_id] {
                            self.temps.get_mut(&node_id).unwrap().var = None;
                        }
                    },
                    VarLocation::StackSlot => {
                        panic!("Variable dirty but already on stack");
                    }
                }
            }
        }
        if let Some(exit_node) = block.exit_node.as_ref() {
            self.generate_node(func_name, &*exit_node.borrow());
        }
    }

    // this consumes codegen itself as codegen can only happen once
    fn codegen(mut self, func: CaplanFunction, ctx: &mut GlobalCodeGenContext) {
        eprintln!("Codegen for function {}", func.name);

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
                    if !self.vars_to_ids.contains_key(&var_name_op) {
                        let var_id = self.vars.len();
                        self.vars_to_ids.insert(var_name_op.clone(), var_id);
                        // initially the variables are not stored anywhere
                        eprintln!("Allocated slot {} to variable {}", self.stack_slots.len(), var_name_op);
                        self.vars.push(VarState {
                            loc: VarLocation::StackSlot,
                            stack_slot: self.stack_slots.len(),
                            dirty: false
                        });
                        self.stack_slots.push(var_id);
                    }
                }
            }
        }

        self.generate_prologue();

        code_printer::print_label(&self.gen_func_label(&func.name));
        // for each basic block, do a topo sort
        for (blk_id, block) in func.dag.blocks.iter().enumerate() {
            eprintln!("Codegen for basic block {}", blk_id);
            self.codegen_block_reset(&func.name, block);
            code_printer::print_label(&self.gen_func_block_label(&func.name, blk_id));
            assert!(self.op_topo_stack.is_empty());
            for node in block.dag.iter() {
                let node_ref = node.borrow();
                if node_ref.dep_count == 0 && !node_ref.cons.is_control_flow() {
                    self.op_topo_stack.push(node.clone());
                }
            }
            while let Some(node) = self.op_topo_stack.pop() {
                let node_ref = node.borrow();
                self.generate_node(&func.name, &*node_ref);
                for rev_dep in node_ref.rev_deps.iter() {
                    let mut rev_dep_m = rev_dep.borrow_mut();
                    rev_dep_m.dep_count -= 1;
                    if rev_dep_m.dep_count == 0 && !rev_dep_m.cons.is_control_flow() {
                        self.op_topo_stack.push(rev_dep.clone());
                    }
                }
            }
            self.codegen_block_end_cleanup(&func.name, block);
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

