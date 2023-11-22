use core::panic;
use std::collections::HashMap;
use std::io::Write;
use std::process::exit;

use crate::dag::*;
use crate::lang::{CaplanFunction, CaplanTranslationUnit};
use crate::utils::GCed;

mod code_printer;
mod arch_defs;

use arch_defs::*;

use self::code_printer::CodePrinter;

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
    gpr_states: [GPRState; GPR_N]
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
            gpr_states: [GPRState::Free; GPR_N]
        }
    }

    fn generate_prologue<T>(&mut self, func_name: &str, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        let func_label = self.gen_func_label(func_name);
        code_printer.print_global_symb(&func_label).unwrap();
        code_printer.print_label(&func_label).unwrap();

        // now we know how much stack space is needed
        let tot_stack_slot_n = self.stack_slots.len() + self.spilled_stack_slots.len() + 1; // we always store ra on stack immediately, hence + 1
        code_printer.print_addi(GPR_IDX_SP, GPR_IDX_SP, -(tot_stack_slot_n as isize * 8)).unwrap();
        code_printer.print_store_to_stack_slot(GPR_IDX_RA, tot_stack_slot_n - 1).unwrap();
    }

    fn generate_epilogue<T>(&mut self, func_name: &str, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        let tot_stack_slot_n = self.stack_slots.len() + self.spilled_stack_slots.len() + 1; // we always store ra on stack immediately, hence + 1
        code_printer.print_load_from_stack_slot(GPR_IDX_RA, tot_stack_slot_n - 1).unwrap();
        code_printer.print_addi(GPR_IDX_SP, GPR_IDX_SP, tot_stack_slot_n as isize * 8).unwrap();
        code_printer.print_ret().unwrap();
    }

    fn spill_reg<T>(&mut self, reg_id: RegId, node_id: IRDAGNodeId, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
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
        code_printer.print_store_to_stack_slot(reg_id, self.spill_slot_to_stack_slot(spill_slot)).unwrap();
        self.spilled_stack_slots[spill_slot] = SpilledStackSlotState::Taken(node_id);
        self.temps.get_mut(&node_id).unwrap().loc = TempLocation::SpilledStackSlot(spill_slot);
        self.gpr_states[reg_id] = GPRState::Free;
    }

    fn spill_any_reg<T>(&mut self, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
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
            self.spill_reg(reg_to_spill, node_to_spill, code_printer);
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
    
    fn assign_reg<T>(&mut self, node_id: IRDAGNodeId, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
        if let Some(reg_id) = self.try_assign_reg_no_spill(node_id) {
            reg_id
        } else {
            let reg_id = self.spill_any_reg(code_printer);
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
                panic!("Failed to unpin GPR {} in state {:?}", reg_id, self.gpr_states[reg_id]);
            }
        }
    }

    fn prepare_source_reg<T>(&mut self, source_node_id: IRDAGNodeId, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
        eprintln!("Prepare source reg {}", source_node_id);
        let temp_state = self.temps.get(&source_node_id).unwrap();
        // bring it to register
        let reg_id = match temp_state.loc {
            TempLocation::GPR(reg_id) => reg_id,
            TempLocation::SpilledStackSlot(spill_slot) => {
                let r = self.assign_reg(source_node_id, code_printer); // this already records the temp location in reg
                self.spilled_stack_slots[spill_slot] = SpilledStackSlotState::Free;
                code_printer.print_load_from_stack_slot(r, self.spill_slot_to_stack_slot(spill_slot)).unwrap();
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

    fn generate_node<T>(&mut self, func_name: &str, node: &IRDAGNode, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        eprintln!("Codegen for node {} ({:?})", node.id, node.cons);
        match &node.cons {
            IRDAGNodeCons::IntConst(val) => {
                let reg_id = self.assign_reg(node.id, code_printer);
                code_printer.print_li(reg_id, *val).unwrap();
            }
            IRDAGNodeCons::IntBinOp(op_type, s1, s2) => {
                let rs1 = self.prepare_source_reg(s1.borrow().id, code_printer);
                let rs2 = self.prepare_source_reg(s2.borrow().id, code_printer);
                self.unpin_gpr(rs1);
                self.unpin_gpr(rs2);
                let rd = self.assign_reg(node.id, code_printer);
                match op_type {
                    IRDAGNodeIntBinOpType::Add => code_printer.print_add(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Sub => code_printer.print_sub(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Mul => code_printer.print_mul(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Div => code_printer.print_div(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::And => code_printer.print_and(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Or => code_printer.print_or(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Xor => code_printer.print_xor(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Eq => code_printer.print_eq(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::NEq => code_printer.print_neq(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::LessThan => code_printer.print_lt(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::GreaterThan => code_printer.print_lt(rd, rs2, rs1).unwrap(),
                    IRDAGNodeIntBinOpType::LessEq => code_printer.print_le(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::GreaterEq => code_printer.print_le(rd, rs1, rs1).unwrap()
                }
            }
            IRDAGNodeCons::IntUnOp(op_type, source) => {
                let rs = self.prepare_source_reg(source.borrow().id, code_printer);
                self.unpin_gpr(rs);
                let rd = self.assign_reg(node.id, code_printer);
                match op_type {
                    IRDAGNodeIntUnOpType::Neg => code_printer.print_neg(rd, rs).unwrap(),
                    IRDAGNodeIntUnOpType::Not => code_printer.print_not(rd, rs).unwrap(),
                    IRDAGNodeIntUnOpType::Negate => code_printer.print_lt(rd, GPR_IDX_X0, rs).unwrap()
                }
            }
            IRDAGNodeCons::Write(var_name, source) => {
                let var_id = *self.vars_to_ids.get(var_name).unwrap();
                let rs = self.prepare_source_reg(source.borrow().id, code_printer);
                self.unpin_gpr(rs);
                let rd = self.assign_reg(node.id, code_printer);
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
                    code_printer.print_mv(rd, rs).unwrap();
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
                        let reg_id = self.assign_reg(node.id, code_printer);
                        let var_state_mut = self.vars.get_mut(var_id).unwrap();
                        var_state_mut.dirty = false; // just loaded, not dirty
                        var_state_mut.loc = VarLocation::GPR(reg_id);
                        code_printer.print_load_from_stack_slot(reg_id, stack_slot).unwrap();
                        reg_id
                    }
                };
                self.temps.get_mut(&node.id).unwrap().var = Some(var_id);
            }
            IRDAGNodeCons::Jump(target) => {
                if let IRDAGNodeCons::Label(Some(blk_id)) = target.borrow().cons {
                    code_printer.print_jump_label(&self.gen_func_block_label(func_name, blk_id)).unwrap();
                } else {
                    panic!("Invalid jump operation with target {:?}", target.borrow().cons);
                }
            }
            IRDAGNodeCons::Branch(target, cond) => {
                let rs = self.prepare_source_reg(cond.borrow().id, code_printer);
                self.unpin_gpr(rs);
                if let IRDAGNodeCons::Label(Some(blk_id)) = target.borrow().cons {
                    code_printer.print_branch_label(&self.gen_func_block_label(func_name, blk_id), rs).unwrap();
                } else {
                    panic!("Invalid branching operation");
                }
            }
            _ => {
                panic!("Unrecognised node {:?}", node.cons);
            }
        }
    }

    // reset for starting unlabeled basic block
    fn codegen_block_unlabeled_reset<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // unlabeled basic block can continue using the existing temporaries
        for node in block.dag.iter() {
            let node_ref = node.borrow();
            self.temps.insert(node_ref.id, TempState {
                var: None,
                loc: TempLocation::Nowhere,
                rev_deps_to_eval: node_ref.rev_deps.len()
            });
        }

        for var_state in self.vars.iter() {
            assert!(!var_state.dirty);
            assert!(matches!(var_state.loc, VarLocation::StackSlot));
        }
    }

    // reset for starting labeled basic block
    fn codegen_block_labeled_reset<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        self.temps.clear();
        self.gpr_states.fill(GPRState::Free);
        // reserve some special registers
        for reserved_gpr in GPR_RESERVED_LIST {
            self.gpr_states[reserved_gpr] = GPRState::Reserved;
        }
        
        for spill_slot_state in self.spilled_stack_slots.iter_mut() {
            *spill_slot_state = SpilledStackSlotState::Free;
        }

        self.codegen_block_unlabeled_reset(func_name, block, code_printer);
    }

    fn codegen_block_reset<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if block.labeled {
            self.codegen_block_labeled_reset(func_name, block, code_printer);
        } else {
            self.codegen_block_unlabeled_reset(func_name, block, code_printer);
        }
    }

    fn codegen_block_end_cleanup<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // write back all dirty variables
        for var_state in self.vars.iter_mut() {
            match var_state.loc {
                VarLocation::GPR(reg_id) => {
                    if var_state.dirty {
                        code_printer.print_store_to_stack_slot(reg_id, var_state.stack_slot).unwrap();
                        var_state.dirty = false;
                    }
                    if let GPRState::Taken(node_id) = self.gpr_states[reg_id] {
                        self.temps.get_mut(&node_id).unwrap().var = None;
                    }
                    var_state.loc = VarLocation::StackSlot;
                },
                VarLocation::StackSlot => {
                    assert!(!var_state.dirty);
                }
            }
        }
        if let Some(exit_node) = block.exit_node.as_ref() {
            self.generate_node(func_name, &*exit_node.borrow(), code_printer);
        }
    }

    // this consumes codegen itself as codegen can only happen once
    fn codegen(mut self, func: CaplanFunction, ctx: &mut GlobalCodeGenContext) {
        eprintln!("Codegen for function {}", func.name);

        let mut prologue_code_printer: CodePrinter<Vec<u8>> = CodePrinter::new(Vec::new());
        let mut main_code_printer : CodePrinter<Vec<u8>> = CodePrinter::new(Vec::new());

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

        // for each basic block, do a topo sort
        for (blk_id, block) in func.dag.blocks.iter().enumerate() {
            eprintln!("Codegen for basic block {}", blk_id);
            self.codegen_block_reset(&func.name, block, &mut main_code_printer);
            main_code_printer.print_label(&self.gen_func_block_label(&func.name, blk_id)).unwrap();
            assert!(self.op_topo_stack.is_empty());
            for node in block.dag.iter() {
                let node_ref = node.borrow();
                if node_ref.dep_count == 0 && !node_ref.cons.is_control_flow() {
                    self.op_topo_stack.push(node.clone());
                }
            }
            while let Some(node) = self.op_topo_stack.pop() {
                let node_ref = node.borrow();
                self.generate_node(&func.name, &*node_ref, &mut main_code_printer);
                for rev_dep in node_ref.rev_deps.iter() {
                    let mut rev_dep_m = rev_dep.borrow_mut();
                    rev_dep_m.dep_count -= 1;
                    if rev_dep_m.dep_count == 0 && !rev_dep_m.cons.is_control_flow() {
                        self.op_topo_stack.push(rev_dep.clone());
                    }
                }
            }
            self.codegen_block_end_cleanup(&func.name, block, &mut main_code_printer);
        }
        
        self.generate_prologue(&func.name, &mut prologue_code_printer);
        self.generate_epilogue(&func.name, &mut main_code_printer);

        let mut stdout = std::io::stdout();
        stdout.write_all(&prologue_code_printer.get_out()).unwrap();
        stdout.write_all(&main_code_printer.get_out()).unwrap();
        writeln!(&mut stdout).unwrap();
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
