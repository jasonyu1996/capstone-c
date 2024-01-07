use core::panic;
use std::collections::HashMap;

use crate::codegen::code_printer::REG_NAMES;
use crate::dag::*;
use crate::lang::{CaplanFunction, CaplanTranslationUnit, CaplanGlobalContext};
use crate::lang_defs::{CaplanType, IntrinsicFunction, CaplanEntryType, CaplanReentryType};
use crate::target_conf::CaplanABI;
use crate::utils::{GCed, align_up_to};

mod code_printer;
mod arch_defs;

use arch_defs::*;

use self::code_printer::CodePrinter;

// maintain the global codegen state
struct GlobalCodeGenContext {
    translation_unit: CaplanTranslationUnit
}

impl GlobalCodeGenContext {
    fn new(translation_unit: CaplanTranslationUnit) -> Self {
        Self { translation_unit }
    }
}


#[derive(Debug)]
enum VarLocation {
    StackSlot,
    GPR(RegId)
}

enum TempLocation {
    SpilledStackSlot(usize, usize),
    GPR(RegId),
    Const(u64), // constant value
    Nowhere
}

#[derive(Clone, Copy, Debug)]
enum GPRState {
    Taken(IRDAGNodeId, usize), // stores the result of node with id
    Pinned(IRDAGNodeId, usize), // pin a reg if it is going to be used immediately
    Reserved,
    Free
}

impl GPRState {
    fn get_node(&self) -> Option<(IRDAGNodeId, usize)> {
        match self {
            Self::Taken(node_id, size) | Self::Pinned(node_id, size) => Some((*node_id, *size)),
            _ => None
        }
    }
}

#[derive(Clone, Copy)]
enum SpilledStackSlotState {
    Taken(IRDAGNodeId),
    Free
}

#[derive(Debug)]
struct VarState {
    loc: VarLocation,
    stack_slot: usize,
    dirty: bool
}

#[derive(Debug)]
struct VarInfo {
    state: VarState,
    ty: CaplanType
}

struct TempState {
    var: Option<VarId>, // if this has the value of a variable
    loc: TempLocation,
    rev_deps_to_eval: usize
}

struct StackSlotInfo {
    offset: usize,
    size: usize
}

// arrangement from high to low: spilled regs, local vars
struct StackFrame {
    stack_slots: Vec<StackSlotInfo>,
    tot_stack_slot_size: usize,
    // assuming 16-byte alignment
    spilled_stack_slots: Vec<SpilledStackSlotState>, // spilled slots are of fixed 8-byte size
}

impl StackFrame {
    fn new() -> Self {
        Self {
            stack_slots: Vec::new(),
            tot_stack_slot_size: 0,
            spilled_stack_slots: Vec::new()
        }
    }

    fn allocate_stack_slot(&mut self, size: usize) -> usize {
        if size > 8 && self.tot_stack_slot_size % 16 != 0 {
            // capability needs 16-byte alignment
            self.tot_stack_slot_size += 8;
        }
        self.stack_slots.push(StackSlotInfo { offset: self.tot_stack_slot_size, size: size });
        self.tot_stack_slot_size += size;
        self.stack_slots.len() - 1
    }

    fn allocate_stack_slot_specified_offset(&mut self, offset: usize, size: usize) -> usize {
        assert!(offset >= self.tot_stack_slot_size && (size <= 8 || offset % 16 == 0), "Invalid specified offset for stack slot");
        self.stack_slots.push(StackSlotInfo { offset: offset, size: size });
        self.tot_stack_slot_size = offset + size;
        self.stack_slots.len() - 1
    }

    fn extend_spill_stack_slots(&mut self, size: usize) -> usize {
        if size == 16 && self.size() % 16 != 0 {
            self.spilled_stack_slots.push(SpilledStackSlotState::Free);
        }
        let res = self.spilled_stack_slots.len();
        self.spilled_stack_slots.push(SpilledStackSlotState::Free);
        if size == 16 {
            self.spilled_stack_slots.push(SpilledStackSlotState::Free);
        }
        res
    }

    fn spill_stack_slot_offset(&self, slot_id: usize) -> usize {
        assert!(slot_id < self.spilled_stack_slots.len());
        self.tot_stack_slot_size + 8 * slot_id
    }

    fn stack_slot_offset(&self, slot_id: usize) -> usize {
        self.stack_slots[slot_id].offset
    }

    // total size
    fn size(&self) -> usize {
        self.tot_stack_slot_size + 8 * self.spilled_stack_slots.len()
    }

    fn find_free_spill_slot(&mut self, size: usize) -> usize {
        let (start, end, step) = if size == 16 {
            if self.tot_stack_slot_size % 16 != 0 {
                (1, self.spilled_stack_slots.len().saturating_sub(1), 2)
            } else {
                (0, self.spilled_stack_slots.len().saturating_sub(1), 2)
            }
        } else {
            (0, self.spilled_stack_slots.len(), 1)
        };
        (start..end).step_by(step).find(|&slot_id| {
            matches!(self.spilled_stack_slots[slot_id], SpilledStackSlotState::Free) &&
                (size == 8 || matches!(self.spilled_stack_slots[slot_id + 1], SpilledStackSlotState::Free))
        }).unwrap_or_else(
            || {
                self.extend_spill_stack_slots(size)
            }
        )
    }

    fn take_spill_slots(&mut self, slot_id: usize, node_id: IRDAGNodeId, size: usize) {
        assert!(matches!(self.spilled_stack_slots[slot_id], SpilledStackSlotState::Free));
        self.spilled_stack_slots[slot_id] = SpilledStackSlotState::Taken(node_id);
        if size == 16 {
            assert!(matches!(self.spilled_stack_slots[slot_id + 1], SpilledStackSlotState::Free));
            self.spilled_stack_slots[slot_id + 1] = SpilledStackSlotState::Taken(node_id);
        }
    }

    fn free_spill_slots(&mut self, slot_id: usize, size: usize) {
        self.spilled_stack_slots[slot_id] = SpilledStackSlotState::Free;
        if size == 16 {
            self.spilled_stack_slots[slot_id + 1] = SpilledStackSlotState::Free;
        }
    }

    fn clear_spill_slots(&mut self) {
        for spill_slot_state in self.spilled_stack_slots.iter_mut() {
            *spill_slot_state = SpilledStackSlotState::Free;
        }
    }
}

type VarId = usize;

// this just generates code for a single function
struct FunctionCodeGen<'ctx> {
    globals: &'ctx CaplanGlobalContext,
    vars_to_ids: HashMap<IRDAGNamedMemLoc, VarId>,
    vars: Vec<VarInfo>,
    stack_frame: StackFrame,
    temps: HashMap<IRDAGNodeId, TempState>,
    op_topo_stack: Vec<GCed<IRDAGNode>>,
    gpr_states: [GPRState; GPR_N],
    // if each GPR is used at all in the function
    gpr_clobbered: [bool; GPR_N],
    gpr_all_clobbered: bool // are all gprs clobbered?
}

impl<'ctx> FunctionCodeGen<'ctx> {
    fn new(globals: &'ctx CaplanGlobalContext) -> Self {
        Self {
            globals: globals,
            vars_to_ids: HashMap::new(),
            vars: Vec::new(),
            stack_frame: StackFrame::new(),
            temps: HashMap::from(
                [(ANON_IRDAG_NODE_ID, TempState {
                    var: None,
                    loc: TempLocation::Nowhere,
                    rev_deps_to_eval: 0
                })]
            ),
            op_topo_stack: Vec::new(),
            gpr_states: [GPRState::Free; GPR_N],
            gpr_clobbered: [false; GPR_N],
            gpr_all_clobbered: false
        }
    }

    fn generate_prologue<T>(&mut self, func: &CaplanFunction, ctx: &GlobalCodeGenContext, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        let cross_dom = matches!(func.entry_type, CaplanEntryType::CrossDom);
        let func_label = self.gen_func_label(&func.name);

        if func.is_naked {
            code_printer.print_global_symb(&func_label).unwrap();
            code_printer.print_label(&func_label).unwrap();
            return;
        }

        if func.reentry_type.needs_reentry() {
            // generates a stub that sets up the stack for re-entry into a
            // domain
            code_printer.print_label(&format!("__{}_reentry", func.name)).unwrap();
            let save_s = matches!(func.reentry_type, CaplanReentryType::SMode);
            self.synchronous_restore_context(0, &[], &[], if save_s { SMODE_CONTEXT_SIZE } else { 0 },
                save_s, true, code_printer);
            if cross_dom {
                code_printer.print_jump_label(&func_label).unwrap();
            }
        }

        if cross_dom {
            code_printer.print_global_symb("_start").unwrap();
            code_printer.print_label("_start").unwrap();
            code_printer.print_label(&format!("__{}_entry", func.name)).unwrap();
        }

        if cross_dom {
            // sp = chunk of initial mem to allocate from
            code_printer.print_ccsrrw(GPR_IDX_SP, GPR_IDX_X0, "cscratch").unwrap();
            // t0 = base, t1 = end
            // TODO: assuming enough space
            // code_printer.print_lcc(GPR_IDX_T0, GPR_IDX_SP, LccField::Base as u64).unwrap();
            code_printer.print_lcc(GPR_IDX_T1, GPR_IDX_SP, LccField::End as u64).unwrap();
            // assuming that the region is 16-byte aligned
            let cap_table_len = ctx.translation_unit.globals.global_vars.len();
            if cap_table_len != 0 {
                code_printer.print_addi(GPR_IDX_T1, GPR_IDX_T1, 
                    -((cap_table_len * self.globals.target_conf.register_width) as isize)).unwrap();
                // gp = cap table
                code_printer.print_split(GPR_IDX_GP, GPR_IDX_SP, GPR_IDX_T1).unwrap();
                // create capability for each global variable (TODO: we now
                // assume no external global variable)
                for (gvar_idx, (_, gvar_type)) in ctx.translation_unit.globals.global_vars.iter().enumerate() {
                    // size rounded up for alignment
                    let gvar_size = align_up_to(gvar_type.size(&self.globals.target_conf),
                        self.globals.target_conf.min_alignment_log);
                    // reserve space
                    code_printer.print_addi(GPR_IDX_T1, GPR_IDX_T1, -(gvar_size as isize)).unwrap();
                    code_printer.print_split(GPR_IDX_T2, GPR_IDX_SP, GPR_IDX_T1).unwrap();
                    // TODO: assuming that all global variables are stored using
                    // non-linear capabilities for now
                    code_printer.print_delin(GPR_IDX_T2).unwrap();
                    // add to the cap table
                    Self::store(GPR_IDX_T2, GPR_IDX_GP, (self.globals.target_conf.register_width * gvar_idx) as isize,
                        self.globals.target_conf.register_width, code_printer);
                }
            }
            // the remaining is used as stack
            // place the cursor at the bottom of the stack (highest address)
                        code_printer.print_scc(GPR_IDX_SP, GPR_IDX_SP, GPR_IDX_T1).unwrap();
            code_printer.print_delin(GPR_IDX_SP).unwrap();
        }

        code_printer.print_global_symb(&func_label).unwrap();
        code_printer.print_label(&func_label).unwrap();

        // now we know how much stack space is needed
        let mut stack_frame_size = align_up_to(self.stack_frame.size(), self.globals.target_conf.min_alignment_log); // this makes sure that all stack frames are aligned

        if cross_dom {
            self.pointer_offset_imm(GPR_IDX_SP, GPR_IDX_SP, -(stack_frame_size as isize), code_printer);
        } else {
            let clobbered_to_save : Vec<_> = GPR_CALLEE_SAVED_LIST.iter().filter(|idx| (self.gpr_all_clobbered || self.gpr_clobbered[**idx])
                && !matches!(self.gpr_states[**idx], GPRState::Reserved)).collect();
            self.pointer_offset_imm(GPR_IDX_SP, GPR_IDX_SP, -((stack_frame_size + self.globals.target_conf.min_alignment * clobbered_to_save.len()) as isize), code_printer);


            for reg_id in clobbered_to_save.into_iter() {
                // clobbered callee-saved registers need saving here
                assert!(!matches!(self.gpr_states[*reg_id], GPRState::Reserved), "Reserved GPR should not be considered as clobbered");
                Self::store(*reg_id, GPR_IDX_SP, stack_frame_size as isize, self.globals.target_conf.min_alignment, code_printer);
                stack_frame_size += self.globals.target_conf.min_alignment;
            }
        }

        assert!(!cross_dom || !func.params.is_empty(), "A domain entry function must have at least one argument");

        // shift params to stack
        for (param_idx, param) in func.params.iter().enumerate() {
            let named_mem_loc = IRDAGNamedMemLoc {
                var_name: param.name.clone(),
                offset: 0
            };
            let var_id = *self.vars_to_ids.get(&named_mem_loc).unwrap();
            if cross_dom || func.reentry_type.needs_reentry() {
                if param_idx == 0 {
                    // the first argument of a domain entry function holds the sealed-return cap
                    assert!(param.ty.is_return_dom(), "Arg 0 of domain entry function must be of return domain type");
                    Self::store(GPR_IDX_RA,
                        GPR_IDX_SP,
                        self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot) as isize,
                        self.vars[var_id].ty.size(&self.globals.target_conf),
                        code_printer
                    );
                } else {
                    Self::store(GPR_PARAMS[param_idx - 1],
                        GPR_IDX_SP,
                        self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot) as isize,
                        self.vars[var_id].ty.size(&self.globals.target_conf),
                        code_printer
                    );
                }
            } else {
                Self::store(GPR_PARAMS[param_idx],
                    GPR_IDX_SP,
                    self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot) as isize,
                    self.vars[var_id].ty.size(&self.globals.target_conf),
                    code_printer
                );
            }
        }
    }

    fn generate_epilogue<T>(&mut self, func: &CaplanFunction, _ctx: &GlobalCodeGenContext, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if func.is_naked {
            return;
        }

        let ret_label = self.gen_func_ret_label(&func.name);
        code_printer.print_label(&ret_label).unwrap();

        if matches!(func.entry_type, CaplanEntryType::CrossDom) {
            // domain entry function needs to return explicitly, so when the end is reached, just loop
            code_printer.print_jump_label(&ret_label).unwrap();
        } else {
            let mut stack_frame_size = align_up_to(self.stack_frame.size(), self.globals.target_conf.min_alignment_log);

            for reg_id in GPR_CALLEE_SAVED_LIST.iter().filter(|idx| (self.gpr_all_clobbered || self.gpr_clobbered[**idx]) &&
                !matches!(self.gpr_states[**idx], GPRState::Reserved)) {
                // clobbered callee-saved registers need restoring here
                // TODO: 16 byte loading
                Self::load(*reg_id, GPR_IDX_SP, stack_frame_size as isize, self.globals.target_conf.min_alignment, code_printer);
                stack_frame_size += self.globals.target_conf.min_alignment;
            }

            self.pointer_offset_imm(GPR_IDX_SP, GPR_IDX_SP, stack_frame_size as isize, code_printer);
            code_printer.print_ret().unwrap();
        }
    }

    fn pointer_offset_imm<T>(&self, rd: RegId, rs: RegId, offset: isize, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if self.reg_size(rs).unwrap() == 8 {
            code_printer.print_addi(rd, rs, offset).unwrap();
        } else {
            code_printer.print_incoffsetimm(rd, rs, offset).unwrap();
        }
    }

    fn pointer_offset<T>(&self, rd: RegId, rs1: RegId, rs2: RegId, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if self.reg_size(rs1).unwrap() == 8 {
            code_printer.print_add(rd, rs1, rs2).unwrap();
        } else {
            code_printer.print_incoffset(rd, rs1, rs2).unwrap();
        }
    }

    // this assumes that the cursor is pointing at the base of the new bound
    fn pointer_bound_imm<T>(&self, rd: RegId, rs: RegId, size: usize, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // do nothing in non-Capstone
        if self.globals.target_conf.min_alignment == 16 {
            code_printer.print_shrinkto(rd, rs, size).unwrap();
        }
    }

    fn reg_size(&self, reg_id: RegId) -> Option<usize> {
        match &self.gpr_states[reg_id] {
            GPRState::Free => None,
            GPRState::Reserved => {
                Some(if self.globals.target_conf.min_alignment == 8 || reg_id == GPR_IDX_X0 || reg_id == GPR_IDX_RA {
                    8
                } else {
                    16
                })
            }
            GPRState::Pinned(_, sz) => Some(*sz),
            GPRState::Taken(_, sz) => Some(*sz)
        }
    }

    fn writeback_if_dirty<T>(&mut self, reg_id: RegId, code_printer: &mut CodePrinter<T>) -> bool where T: std::io::Write {
        match &self.gpr_states[reg_id] {
            GPRState::Taken(node_id, size) => {
                if let Some(var_id) = self.temps.get(node_id).unwrap().var {
                    if self.vars[var_id].state.dirty {
                        // need to write back if it's dirty
                        let slot_id = self.vars[var_id].state.stack_slot;
                        Self::store(reg_id, GPR_IDX_SP, self.stack_frame.stack_slot_offset(slot_id) as isize, *size, code_printer);
                    }
                    self.temps.get_mut(node_id).unwrap().loc = TempLocation::Nowhere;
                    self.vars[var_id].state.loc = VarLocation::StackSlot;
                    self.vars[var_id].state.dirty = false;
                    self.gpr_states[reg_id] = GPRState::Free;
                    true
                } else {
                    false
                }
            }
            _ => panic!("GPR not taken")
        }
    }

    fn spill_reg<T>(&mut self, reg_id: RegId, node_id: IRDAGNodeId, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // find a spill slot
        if !self.writeback_if_dirty(reg_id, code_printer) {
            let size = self.reg_size(reg_id).unwrap();

            let spill_slot = self.stack_frame.find_free_spill_slot(size);
            // TODO: 16-byte spilling
            Self::store(reg_id, GPR_IDX_SP, self.stack_frame.spill_stack_slot_offset(spill_slot) as isize, size, code_printer);
            self.stack_frame.take_spill_slots(spill_slot, node_id, size);
            self.temps.get_mut(&node_id).unwrap().loc = TempLocation::SpilledStackSlot(spill_slot, size);
            self.gpr_states[reg_id] = GPRState::Free;
        }
    }

    fn spill_reg_if_taken<T>(&mut self, reg_id: RegId, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        match self.gpr_states[reg_id] {
            GPRState::Free => (),
            GPRState::Taken(node_id, _) => {
                let temp_state = self.temps.get_mut(&node_id).unwrap();
                if temp_state.rev_deps_to_eval == 0 {
                    temp_state.loc = TempLocation::Nowhere;
                                        self.writeback_if_dirty(reg_id, code_printer);
                    self.gpr_states[reg_id] = GPRState::Free;
                } else {
                    self.spill_reg(reg_id, node_id, code_printer);
                }
                assert!(matches!(self.gpr_states[reg_id], GPRState::Free));
            }
            GPRState::Reserved => panic!("Attempting to spill reserved reg"),
            GPRState::Pinned(node_id, _) => panic!("Attempting to spill pinned reg for node {}", node_id)
        }
    }
    
    fn reg_is_dead(&self, gpr_state: &GPRState) -> bool {
        match gpr_state {
            GPRState::Taken(node_id, _) => {
                self.temps.get(node_id).unwrap().rev_deps_to_eval == 0
            }
            GPRState::Pinned(node_id, _) => {
                self.temps.get(node_id).unwrap().rev_deps_to_eval == 0 // FIXME: not done correctly
            }
            GPRState::Reserved => false,
            GPRState::Free => true
        }
    }

    fn spill_any_reg<T>(&mut self, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
        let dead = self.gpr_states.iter().enumerate().find(
            |(_, s)| self.reg_is_dead(*s)
        ).map(|(idx, _)| idx);
        if let Some(dead_reg) = dead {
            // no need to write back either
            dead_reg
        } else {
            let (reg_to_spill, &mut node_to_spill) = self.gpr_states.iter_mut().enumerate().find_map(|(idx, s)| {
                if let GPRState::Taken(node_id, _) = s {
                    Some((idx, node_id))
                } else {
                    None
                }
            }).unwrap();
            self.spill_reg(reg_to_spill, node_to_spill, code_printer);
            reg_to_spill
        }
    }

    fn try_assign_reg_no_spill(&mut self, node_id: IRDAGNodeId, size: usize) -> Option<RegId> {
        self.gpr_states.iter_mut().enumerate().find(|(_, s)| matches!(**s, GPRState::Free)).map(
            |(idx, s)| {
                *s = GPRState::Taken(node_id, size);
                self.gpr_clobbered[idx] = true;
                self.temps.get_mut(&node_id).map(|t| t.loc = TempLocation::GPR(idx));
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

    fn gen_func_ret_label(&self, func_name: &str) -> String {
        format!("_{}.ret", func_name)
    }
    
    fn assign_reg<T>(&mut self, node_id: IRDAGNodeId, size: usize, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
        if let Some(reg_id) = self.try_assign_reg_no_spill(node_id, size) {
            reg_id
        } else {
            let reg_id = self.spill_any_reg(code_printer);
            self.gpr_states[reg_id] = GPRState::Taken(node_id, size); // initially it is pinned to prevent immediate spilling
            self.gpr_clobbered[reg_id] = true;
            self.temps.get_mut(&node_id).unwrap().loc = TempLocation::GPR(reg_id);
            reg_id
        }
    }

    fn assign_reg_with_hint<T>(&mut self, node_id: IRDAGNodeId, size: usize, hint: RegId, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
        if self.reg_is_dead(&self.gpr_states[hint]) {
            self.spill_reg_if_taken(hint, code_printer);
            self.gpr_states[hint] = GPRState::Taken(node_id, size); // initially it is pinned to prevent immediate spilling
            self.gpr_clobbered[hint] = true;
            self.temps.get_mut(&node_id).unwrap().loc = TempLocation::GPR(hint);
            hint
        } else {
            self.assign_reg(node_id, size, code_printer)
        }
    }

    fn pin_gpr(&mut self, reg_id: RegId) {
        match self.gpr_states[reg_id] {
            GPRState::Taken(node_id, size) => {
                self.gpr_states[reg_id] = GPRState::Pinned(node_id, size);
            }
            GPRState::Pinned(_, _) => (),
            _ => {
                panic!("Failed to pin GPR {} in state {:?}", reg_id, self.gpr_states[reg_id]);
            }
        }
    }

    fn unpin_gpr(&mut self, reg_id: RegId) {
        match self.gpr_states[reg_id] {
            GPRState::Taken(_, _) => (),
            GPRState::Pinned(node_id, size) => {
                self.gpr_states[reg_id] = GPRState::Taken(node_id, size);
            }
            _ => {
                panic!("Failed to unpin GPR {} in state {:?}", reg_id, self.gpr_states[reg_id]);
            }
        }
    }


    fn move_reg<T>(&self, rd: RegId, rs: RegId, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        let size = self.reg_size(rs).unwrap();
        if size == 8 {
            code_printer.print_mv(rd, rs)
        } else {
            code_printer.print_movc(rd, rs)
        }.unwrap();
    }

    fn load<T>(rd: RegId, rs: RegId, offset: isize, size: usize, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if size == 8 {
            code_printer.print_ld(rd, rs, offset).unwrap();
        } else {
            code_printer.print_ldc(rd, rs, offset).unwrap();
        }
    }

    fn store<T>(rs1: RegId, rs2: RegId, offset: isize, size: usize, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if size == 8 {
            code_printer.print_sd(rs1, rs2, offset).unwrap();
        } else {
            code_printer.print_stc(rs1, rs2, offset).unwrap();
        }
    }


    fn prepare_source_reg_specified<T>(&mut self, source_node: &IRDAGNode,
            target_reg_id: RegId, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        let source_node_id = source_node.id;
        let temp_state = self.temps.get(&source_node_id).unwrap();
        let size = match temp_state.loc {
            TempLocation::GPR(reg_id) => {
                let res = self.reg_size(reg_id).unwrap();
                if target_reg_id != reg_id {
                    self.spill_reg_if_taken(target_reg_id, code_printer);
                    // just move
                    self.move_reg(target_reg_id, reg_id, code_printer);
                }
                res
            }
            TempLocation::SpilledStackSlot(spill_slot, size) => {
                self.spill_reg_if_taken(target_reg_id, code_printer);
                Self::load(target_reg_id, GPR_IDX_SP, self.stack_frame.spill_stack_slot_offset(spill_slot) as isize, size, code_printer);
                self.stack_frame.free_spill_slots(spill_slot, size);
                size
            }
            TempLocation::Const(const_val) => {
                self.spill_reg_if_taken(target_reg_id, code_printer);
                code_printer.print_li(target_reg_id, const_val).unwrap();
                8
            }
            TempLocation::Nowhere => {
                if let Some(var_id) = temp_state.var {
                    let size = self.vars[var_id].ty.size(&self.globals.target_conf);
                    self.spill_reg_if_taken(target_reg_id, code_printer);
                    Self::load(target_reg_id, GPR_IDX_SP, self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot) as isize, size, code_printer);
                    size
                } else {
                    panic!("Nowhere to find the source node");
                }
            }
        };
        let temp_state = self.temps.get_mut(&source_node_id).unwrap();
        temp_state.loc = TempLocation::GPR(target_reg_id);
        temp_state.rev_deps_to_eval -= 1;
        if let Some(var_id) = temp_state.var {
            self.vars[var_id].state.loc = VarLocation::GPR(target_reg_id);
        }
        self.gpr_states[target_reg_id] = GPRState::Pinned(source_node_id, size);
    }


    fn prepare_source_reg<T>(&mut self, source_node: &IRDAGNode, code_printer: &mut CodePrinter<T>) -> RegId where T: std::io::Write {
        let source_node_id = source_node.id;
        eprintln!("Prepare reg for source node {} of type {:?}", source_node_id, source_node.vtype);
        let temp_state = self.temps.get(&source_node_id).unwrap();
        // bring it to register
        let (reg_id, size) = match temp_state.loc {
            TempLocation::GPR(reg_id) => (reg_id, self.reg_size(reg_id).unwrap()),
            TempLocation::SpilledStackSlot(spill_slot, size) => {
                let r = self.assign_reg(source_node_id, size, code_printer); // this already records the temp location in reg
                self.stack_frame.free_spill_slots(spill_slot, size);
                Self::load(r, GPR_IDX_SP, self.stack_frame.spill_stack_slot_offset(spill_slot) as isize, size, code_printer);
                (r, size)
            }
            TempLocation::Const(const_val) => {
                if const_val == 0 {
                    self.temps.get_mut(&source_node_id).unwrap().loc = TempLocation::GPR(GPR_IDX_X0);
                    (GPR_IDX_X0, 8)
                } else {
                    let reg_id = self.assign_reg(source_node_id, 8, code_printer);
                    code_printer.print_li(reg_id, const_val).unwrap();
                    (reg_id, 8)
                }
            }
            TempLocation::Nowhere => {
                if let Some(var_id) = temp_state.var {
                    let v = &self.vars[var_id];
                    let size = v.ty.size(&self.globals.target_conf);
                    let reg_id = match &v.state.loc {
                        VarLocation::GPR(reg_id) => *reg_id,
                        VarLocation::StackSlot => {
                            let r = self.assign_reg(source_node_id, size, code_printer); // this already records the temp location in reg
                            // self.spill_reg_if_taken(r, code_printer);
                            Self::load(r, GPR_IDX_SP, self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot) as isize, size, code_printer);
                            self.vars[var_id].state.loc = VarLocation::GPR(r);
                            self.vars[var_id].state.dirty = self.vars[var_id].ty.is_linear();
                            r
                        }
                    };
                    (reg_id, size)
                } else {
                    panic!("Nowhere to find the source node");
                }
            }
        };
        let temp_state = self.temps.get_mut(&source_node_id).unwrap();
        temp_state.loc = TempLocation::GPR(reg_id);
        temp_state.rev_deps_to_eval -= 1;
        self.gpr_states[reg_id] = GPRState::Pinned(source_node_id, size);
        reg_id
    }

    fn get_global_var_pointer<T>(&self, reg: RegId, var_name: &str, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        match &self.globals.target_conf.abi {
            CaplanABI::RISCV64 => {
                code_printer.print_la(reg, var_name).unwrap();
            }
            CaplanABI::CapstoneCGNLSD => {
                let global_var_idx = *self.globals.global_vars_to_ids.get(var_name).unwrap();
                code_printer.print_ldc(reg, GPR_IDX_GP, (16 * global_var_idx) as isize).unwrap();
            }
        }
    }

    // this assumes that the value is currently in a register
    fn destruct_temp_value(&mut self, reg: RegId, node: &IRDAGNode) {
        let temp_ref = self.temps.get_mut(&node.id).unwrap();
        temp_ref.loc = TempLocation::Nowhere;
        if let Some(var_id) = &temp_ref.var {
            self.vars[*var_id].state.dirty = false;
            self.vars[*var_id].state.loc = VarLocation::StackSlot;
        }
        self.gpr_states[reg] = GPRState::Free; 
    }

    // returns the size of the saved context
    // rd: register for holding the new domain
    fn synchronous_save_context<T>(&mut self, args_count: usize, rd: &[RegId], rs: &[RegId],
            reserved_stack_space: usize, save_s: bool, dom_boundary: bool, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // TODO: for now everything is done inline, but we might want to consider gathering all these in one place for smaller code size
        // TODO: more CSRs and CCSRs in the domain scope
        let arg_reg_lo = GPR_IDX_A0;
        let arg_reg_hi = GPR_IDX_A0 + args_count;
        let mut neg_stack_offset = reserved_stack_space;

        let mut tmp_reg = GPR_IDX_T0;
        while rd.contains(&tmp_reg) || rs.contains(&tmp_reg) || tmp_reg == GPR_IDX_SP || matches!(self.gpr_states[tmp_reg], GPRState::Reserved) {
            tmp_reg += 1;
        }
        // GPRs
        
        let save_gpr_iter = get_save_gpr_iter(dom_boundary);
        for gpr in save_gpr_iter {
            assert!(!rd.contains(gpr) && !rs.contains(gpr));
            neg_stack_offset += self.globals.target_conf.register_width;
            Self::store(*gpr, GPR_IDX_SP, -(neg_stack_offset as isize), self.globals.target_conf.register_width, code_printer);
        }

        for reg in 1..GPR_N {
            if reg == GPR_IDX_SP || (reg >= arg_reg_lo && reg < arg_reg_hi) {
                continue;
            }
            if !rs.contains(&reg) {
                code_printer.print_li(reg, 0).unwrap(); // scrub
            }
        }

        let (save_ccsr_iter, save_csr_iter) = get_dom_save_context(save_s);

        for ccsr in save_ccsr_iter {
            code_printer.print_ccsrrw(tmp_reg, GPR_IDX_X0, ccsr).unwrap();
            neg_stack_offset += self.globals.target_conf.register_width;
            Self::store(tmp_reg, GPR_IDX_SP, -(neg_stack_offset as isize), self.globals.target_conf.register_width, code_printer);
        }

        for csr in save_csr_iter {
            code_printer.print_csrrw(tmp_reg, GPR_IDX_X0, csr).unwrap();
            neg_stack_offset += 8;
            Self::store(tmp_reg, GPR_IDX_SP, -(neg_stack_offset as isize), 8, code_printer);
        }

        code_printer.print_li(tmp_reg, 0).unwrap();
        code_printer.print_ccsrrw(GPR_IDX_X0, GPR_IDX_SP, "cscratch").unwrap();
        code_printer.print_li(GPR_IDX_SP, 0).unwrap();
    }

    fn synchronous_restore_context<T>(&mut self, args_count: usize, rd: &[RegId], rs: &[RegId],
            reserved_stack_space: usize, save_s: bool, dom_boundary: bool, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        let mut neg_stack_offset = 0;

        // load sp back
        code_printer.print_ccsrrw(GPR_IDX_SP, GPR_IDX_X0, "cscratch").unwrap();
        // restore the original cscratch

        if reserved_stack_space != 0 {
            code_printer.print_incoffsetimm(GPR_IDX_SP, GPR_IDX_SP, -(reserved_stack_space as isize)).unwrap();
        }
        let save_gpr_iter = get_save_gpr_iter(dom_boundary);
        for gpr in save_gpr_iter {
            assert!(!rd.contains(gpr) && !rs.contains(gpr));
            neg_stack_offset += self.globals.target_conf.register_width;
            Self::load(*gpr, GPR_IDX_SP, -(neg_stack_offset as isize), self.globals.target_conf.register_width, code_printer);
        }

        let (save_ccsr_iter, save_csr_iter) = get_dom_save_context(save_s);

        let mut tmp_reg = GPR_IDX_T0;
        while rd.contains(&tmp_reg) || rs.contains(&tmp_reg) || tmp_reg == GPR_IDX_SP || matches!(self.gpr_states[tmp_reg], GPRState::Reserved) {
            tmp_reg += 1;
        }

        for ccsr in save_ccsr_iter {
            neg_stack_offset += self.globals.target_conf.register_width;
            Self::load(tmp_reg, GPR_IDX_SP, -(neg_stack_offset as isize), self.globals.target_conf.register_width, code_printer);
            code_printer.print_ccsrrw(GPR_IDX_X0, tmp_reg, ccsr).unwrap();
        }

        for csr in save_csr_iter {
            neg_stack_offset += 8;
            Self::load(tmp_reg, GPR_IDX_SP, -(neg_stack_offset as isize), 8, code_printer);
            code_printer.print_csrrw(GPR_IDX_X0, tmp_reg, csr).unwrap();
        }
    }

    // only for synchronous return
    fn domreturn_save_context<T>(&mut self, rs1: RegId, rs2: RegId, reserved_stack_space: usize, save_s: bool, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // TODO: support return value here
        // store stack pointer on cscratch
        let mut tmp_reg = 1;
        while tmp_reg == rs1 || tmp_reg == rs2 || tmp_reg == GPR_IDX_SP || matches!(self.gpr_states[tmp_reg], GPRState::Reserved) {
            tmp_reg += 1;
        }
        assert!(tmp_reg < GPR_N);

        // reset stack pointer to top
        code_printer.print_lcc(tmp_reg, GPR_IDX_SP, LccField::End as u64).unwrap();
        code_printer.print_scc(GPR_IDX_SP, GPR_IDX_SP, tmp_reg).unwrap();

        self.synchronous_save_context(0, &[], &[rs1, rs2], reserved_stack_space, save_s, true, code_printer);
    }

    // write_through: write back directly to memory, ignoring the registers
    fn gen_writeback_to_loc<T>(&mut self, loc: &IRDAGMemLoc, reg: RegId, size: usize, write_through: bool, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        match loc {
            IRDAGMemLoc::Addr(addr, static_offset, dyn_offset_op) => {
                let rs = self.prepare_source_reg(&*addr.borrow(), code_printer);
                if let Some(dyn_offset) = dyn_offset_op {
                    // this requires us to write back to memory regardless
                    if addr.borrow().vtype.is_linear() {
                        let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                        let r_addr = self.assign_reg(ANON_IRDAG_NODE_ID, addr.borrow().vtype.size(), code_printer);
                        self.pin_gpr(r_addr);
                        self.pointer_offset(r_addr, rs, r_offset, code_printer);
                        Self::store(reg, r_addr, *static_offset as isize, size, code_printer);
                        self.unpin_gpr(r_offset);
                        // restore the capability in rs
                        let r_neg_offset = self.assign_reg_with_hint(ANON_IRDAG_NODE_ID, 8, r_offset, code_printer);
                        code_printer.print_sub(r_neg_offset, GPR_IDX_X0, r_offset).unwrap();
                        self.pointer_offset(rs, r_addr, r_neg_offset, code_printer);
                        self.unpin_gpr(rs);
                        self.unpin_gpr(r_addr);
                    } else {
                        let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                        self.unpin_gpr(rs);
                        self.unpin_gpr(r_offset);
                        let r_addr = self.assign_reg(ANON_IRDAG_NODE_ID, addr.borrow().vtype.size(), code_printer);
                        self.pointer_offset(r_addr, rs, r_offset, code_printer);
                        Self::store(reg, r_addr, *static_offset as isize, size, code_printer);
                    }
                } else {
                    self.unpin_gpr(rs);
                    Self::store(reg, rs, *static_offset as isize, size, code_printer);
                }
            }
            IRDAGMemLoc::Named(named_mem_loc) => {
                if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                    if write_through {
                        Self::store(reg, GPR_IDX_SP,
                            self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot) as isize,
                            size, code_printer);
                    } else {
                        match &self.vars[var_id].state.loc {
                            VarLocation::GPR(r) => assert_eq!(*r, reg),
                            _ => panic!("Bad register to write back")
                        }
                        self.vars[var_id].state.dirty = true; // TODO: not quite correct if the original read value is still needed
                    }
                } else {
                    let rs = self.assign_reg(ANON_IRDAG_NODE_ID, self.globals.target_conf.register_width, code_printer);
                    self.unpin_gpr(rs);
                    self.get_global_var_pointer(rs, &named_mem_loc.var_name, code_printer);
                    Self::store(reg, rs, named_mem_loc.offset as isize, size, code_printer);
                }
            }
            IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, dyn_offset, range) => {
                let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                self.unpin_gpr(r_offset);
                if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                    if write_through {
                        let rd = self.assign_reg_with_hint(ANON_IRDAG_NODE_ID, self.globals.target_conf.register_width,
                            r_offset, code_printer);
                        self.pointer_offset(rd, GPR_IDX_SP, r_offset, code_printer);
                        Self::store(reg, rd,
                            (named_mem_loc.offset + self.stack_frame.stack_slot_offset(self.vars[var_id].state.stack_slot)) as isize,
                            size, code_printer);
                    } else {
                        match &self.vars[var_id].state.loc {
                            VarLocation::GPR(r) => assert_eq!(*r, reg),
                            _ => panic!("Bad register to write back")
                        }
                        self.vars[var_id].state.dirty = true; // TODO: not quite correct if the original read value is still needed
                    }
                } else {
                    let rs = self.assign_reg(ANON_IRDAG_NODE_ID, self.globals.target_conf.register_width, code_printer);
                    self.unpin_gpr(rs);
                    self.get_global_var_pointer(rs, &named_mem_loc.var_name, code_printer);
                    self.pointer_offset(rs, rs, r_offset, code_printer);
                    Self::store(reg, rs, named_mem_loc.offset as isize, size, code_printer);
                }
            }
        }
    }

    fn discard_var_state(&mut self, var_id: VarId) {
        let v = &mut self.vars[var_id];
        match v.state.loc {
            VarLocation::GPR(gpr) => {
                v.state.dirty = false;
                if let Some((node_id, _)) = self.gpr_states[gpr].get_node() {
                    self.temps.get_mut(&node_id).unwrap().loc = TempLocation::Nowhere;
                }
                self.gpr_states[gpr] = GPRState::Free;
            }
            VarLocation::StackSlot => ()
        }
        v.state.loc = VarLocation::StackSlot;
    }

    fn generate_node<T>(&mut self, func_name: &str, node: &IRDAGNode, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        eprintln!("Codegen for node {} ({:?})", node.id, node.cons);
        if node.rev_deps.is_empty() && !node.side_effects {
            eprintln!("Skipped");
            return;
        }
        eprintln!("Rev dep count = {}, side effects = {}", node.rev_deps.len(), node.side_effects);
        match &node.cons {
            IRDAGNodeCons::IntConst(val) => {
                self.temps.get_mut(&node.id).unwrap().loc = TempLocation::Const(*val);
            }
            IRDAGNodeCons::IntBinOp(op_type, s1, s2) => {
                let rs1 = self.prepare_source_reg(&*s1.borrow(), code_printer);
                let rs2 = self.prepare_source_reg(&*s2.borrow(), code_printer);
                assert_eq!(s2.borrow().vtype.size(), 8);
                let res_size = node.vtype.size();
                self.unpin_gpr(rs1);
                self.unpin_gpr(rs2);

                if s1.borrow().vtype.is_linear() {
                    // ok the original value is garbage now
                    self.gpr_states[rs1] = GPRState::Free;
                    let temp_ref = self.temps.get_mut(&s1.borrow().id).unwrap();
                    temp_ref.loc = TempLocation::Nowhere;
                    if let Some(var_id) = temp_ref.var {
                        self.vars[var_id].state.dirty = false;
                        self.vars[var_id].state.loc = VarLocation::StackSlot; /* Actually should be nowhere */
                    }
                }

                let rd = self.assign_reg_with_hint(node.id, res_size, rs1, code_printer);
                // let rd = self.assign_reg(node.id, res_size, code_printer);
                match op_type {
                    IRDAGNodeIntBinOpType::Add => {
                        if res_size == 8 {
                            code_printer.print_add(rd, rs1, rs2).unwrap();
                        } else {
                            code_printer.print_incoffset(rd, rs1, rs2).unwrap();
                        }
                    }
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
                    IRDAGNodeIntBinOpType::GreaterEq => code_printer.print_le(rd, rs2, rs1).unwrap(),
                    IRDAGNodeIntBinOpType::Shl => code_printer.print_sll(rd, rs1, rs2).unwrap(),
                    IRDAGNodeIntBinOpType::Shr => code_printer.print_srl(rd, rs1, rs2).unwrap()
                }
            }
            IRDAGNodeCons::IntUnOp(op_type, source) => {
                let res_size = source.borrow().vtype.size();
                let rs = self.prepare_source_reg(&*source.borrow(), code_printer);
                self.unpin_gpr(rs);
                let rd = self.assign_reg_with_hint(node.id, res_size, rs, code_printer);
                match op_type {
                    IRDAGNodeIntUnOpType::Neg => code_printer.print_neg(rd, rs).unwrap(),
                    IRDAGNodeIntUnOpType::Not => code_printer.print_not(rd, rs).unwrap(),
                    IRDAGNodeIntUnOpType::Negate => code_printer.print_eq(rd, GPR_IDX_X0, rs).unwrap()
                }
            }
            IRDAGNodeCons::AddressOf(named_mem_loc) => {
                let rd = self.assign_reg(node.id, self.globals.target_conf.register_width, code_printer);
                if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                    // TODO: capability needs special treatment
                    self.pointer_offset_imm(rd, GPR_IDX_SP,
                        self.stack_frame.stack_slot_offset(self.vars.get(var_id).unwrap().state.stack_slot) as isize, code_printer);
                    self.pointer_bound_imm(rd, rd, node.vtype.inner_type().unwrap().size(&self.globals.target_conf), code_printer)
                } else {
                    self.get_global_var_pointer(rd, &named_mem_loc.var_name, code_printer);
                    if named_mem_loc.offset != 0 {
                        self.pointer_offset_imm(rd, rd, named_mem_loc.offset as isize, code_printer);
                    }
                }
            }
            IRDAGNodeCons::Write(loc, source) => {
                eprintln!("Write value of type {:?}", source.borrow().vtype);
                let v_size = source.borrow().vtype.size();
                match &loc {
                    IRDAGMemLoc::Named(named_mem_loc) => {
                        let rs = self.prepare_source_reg(&*source.borrow(), code_printer);
                        if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                                                        self.unpin_gpr(rs);
                            let rd = self.assign_reg_with_hint(node.id, v_size, rs, code_printer);
                            let var_info = self.vars.get_mut(var_id).unwrap();
                            // look at current location of the variable
                            if let VarLocation::GPR(old_reg_id) = var_info.state.loc {
                                // disassociate the old reg and node
                                if let GPRState::Taken(node_id, _) = self.gpr_states[old_reg_id] {
                                    self.temps.get_mut(&node_id).unwrap().var = None;
                                }
                            }
                            
                            var_info.state.loc = VarLocation::GPR(rd);
                            var_info.state.dirty = true;
                            self.temps.get_mut(&node.id).unwrap().var = Some(var_id);

                            if rd != rs {
                                self.move_reg(rd, rs, code_printer);
                            }
                        } else {
                            let rd = self.assign_reg(node.id, self.globals.target_conf.register_width, code_printer); // FIXME: the result doesn't actually reside here
                            self.unpin_gpr(rs);
                            self.get_global_var_pointer(rd, &named_mem_loc.var_name, code_printer);
                            Self::store(rs, rd, named_mem_loc.offset as isize, v_size, code_printer);
                        }
                                            }
                    IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, dyn_offset, offset_range) => {
                        // FIXME: invalidate all variables in register covered in offset range

                        let r_val = self.prepare_source_reg(&*source.borrow(), code_printer);
                        let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                        self.unpin_gpr(r_offset);
                        if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                            let rd = self.assign_reg_with_hint(node.id, self.globals.target_conf.register_width, r_offset, code_printer);
                            self.unpin_gpr(r_val);

                            self.pointer_offset(rd, GPR_IDX_SP, r_offset, code_printer);
                            Self::store(r_val, rd, (named_mem_loc.offset +
                                self.stack_frame.stack_slot_offset(self.vars.get(var_id).unwrap().state.stack_slot)) as isize, v_size, code_printer);
                        } else {
                            let rd = self.assign_reg(node.id, self.globals.target_conf.register_width, code_printer); // FIXME: need to set the result reg correctly
                            self.unpin_gpr(r_val);
                            self.get_global_var_pointer(rd, &named_mem_loc.var_name, code_printer);
                            self.pointer_offset(rd, rd, r_offset, code_printer);
                            Self::store(r_val, rd, named_mem_loc.offset as isize, v_size, code_printer);
                        }
                    }
                    IRDAGMemLoc::Addr(addr, static_offset, dyn_offset_op) => {
                        let rs1 = self.prepare_source_reg(&*source.borrow(), code_printer);
                        let rs2 = self.prepare_source_reg(&*addr.borrow(), code_printer);
                        if let Some(dyn_offset) = dyn_offset_op {
                            let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                            if addr.borrow().vtype.is_linear() {
                                let r_addr = self.assign_reg(node.id, addr.borrow().vtype.size(), code_printer);
                                self.pin_gpr(r_addr);
                                self.pointer_offset(r_addr, rs2, r_offset, code_printer);
                                Self::store(rs1, r_addr, *static_offset as isize, v_size, code_printer);
                                // restore capability
                                self.unpin_gpr(r_offset);
                                let r_neg_offset = self.assign_reg_with_hint(node.id, 8, r_offset, code_printer);
                                code_printer.print_sub(r_neg_offset, GPR_IDX_X0, r_offset).unwrap();
                                self.pointer_offset(rs2, r_addr, r_neg_offset, code_printer);
                                self.unpin_gpr(rs1);
                                self.unpin_gpr(rs2);
                                self.unpin_gpr(r_addr);
                            } else {
                                self.unpin_gpr(rs2);
                                self.unpin_gpr(r_offset);
                                let r_addr = self.assign_reg(node.id, addr.borrow().vtype.size(), code_printer);
                                self.pointer_offset(r_addr, rs2, r_offset, code_printer);
                                self.unpin_gpr(rs1);
                                Self::store(rs1, r_addr, *static_offset as isize, v_size, code_printer);
                            }
                        } else {
                            self.unpin_gpr(rs1);
                            self.unpin_gpr(rs2);
                            Self::store(rs1, rs2, *static_offset as isize, v_size, code_printer);
                        }
                    }
                }
            }
            IRDAGNodeCons::Read(mem_loc) => {
                // see where it is right now
                eprintln!("Read value with type {:?}", node.vtype);
                let res_size = node.vtype.size();
                match mem_loc {
                    IRDAGMemLoc::Named(named_mem_loc) => {
                        // assert!(res_size == 8, "loading capability from named location not implemented");

                        if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                            let var_info = self.vars.get(var_id).unwrap();
                            // local variable

                            // look at current location of the variable
                            match var_info.state.loc {
                                VarLocation::GPR(reg_id) => {
                                    // already in a register
                                    // just grab the register
                                    if let GPRState::Taken(node_id, _) = self.gpr_states[reg_id] {
                                        self.temps.get_mut(&node_id).unwrap().var = None; // disassociate with old node
                                    }
                                    self.temps.get_mut(&node.id).unwrap().loc = TempLocation::GPR(reg_id);
                                    self.gpr_states[reg_id] = GPRState::Taken(node.id, node.vtype.size());
                                }
                                VarLocation::StackSlot => {
                                    // need to load from stack
                                    // let stack_slot = var_info.state.stack_slot;
                                    // let reg_id = self.assign_reg(node.id, res_size, code_printer);
                                    // let var_info_mut = self.vars.get_mut(var_id).unwrap();
                                    // var_info_mut.state.dirty = node.vtype.is_linear(); // just loaded, not dirty, but writeback is necessary if the read value is linear
                                    // var_info_mut.state.loc = VarLocation::GPR(reg_id);
                                    // Self::load(reg_id, GPR_IDX_SP, self.stack_frame.stack_slot_offset(stack_slot) as isize, res_size, code_printer);
                                    
                                    // only load lazily later
                                }
                            };
                            self.temps.get_mut(&node.id).unwrap().var = Some(var_id);
                        } else {
                            // global variable
                            let rd = self.assign_reg(node.id, res_size, code_printer);
                            self.get_global_var_pointer(rd, &named_mem_loc.var_name, code_printer);
                            Self::load(rd, rd, named_mem_loc.offset as isize, res_size, code_printer);
                        }
                    }
                    IRDAGMemLoc::NamedWithDynOffset(named_mem_loc, dyn_offset, offset_range) => {
                        // FIXME: write back everything dirty in offset range

                        let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                        // get address
                        if let Some(&var_id) = self.vars_to_ids.get(named_mem_loc) {
                            self.unpin_gpr(r_offset);
                            let rd = self.assign_reg_with_hint(node.id, res_size, r_offset, code_printer);
                            self.pointer_offset(rd, GPR_IDX_SP, r_offset, code_printer);
                            Self::load(rd, rd, 
                                (named_mem_loc.offset + self.stack_frame.stack_slot_offset(self.vars.get(var_id).unwrap().state.stack_slot)) as isize, res_size, code_printer);
                        } else {
                            let rd = self.assign_reg(node.id, res_size, code_printer);
                            self.unpin_gpr(r_offset);
                            self.get_global_var_pointer(rd, &named_mem_loc.var_name, code_printer);
                            self.pointer_offset(rd, rd, r_offset, code_printer);
                            Self::load(rd, rd, named_mem_loc.offset as isize, res_size, code_printer);
                        }
                    }
                    IRDAGMemLoc::Addr(addr, static_offset, dyn_offset_op) => {
                        // TODO: if the address is linear, we would need to somehow restore it
                        let rs = self.prepare_source_reg(&*addr.borrow(), code_printer);
                        if let Some(dyn_offset) = dyn_offset_op {
                            let r_offset = self.prepare_source_reg(&*dyn_offset.borrow(), code_printer);
                            if addr.borrow().vtype.is_linear() {
                                let r_addr = self.assign_reg(node.id, addr.borrow().vtype.size(), code_printer);
                                self.pin_gpr(r_addr);
                                let reg_id = self.assign_reg(node.id, res_size, code_printer);
                                self.pointer_offset(reg_id, rs, r_offset, code_printer);
                                // restore capability
                                self.unpin_gpr(r_offset);
                                let r_neg_offset = self.assign_reg_with_hint(node.id, 8, r_offset, code_printer);
                                code_printer.print_sub(r_neg_offset, GPR_IDX_X0, r_offset).unwrap();
                                self.pointer_offset(rs, r_addr, r_neg_offset, code_printer);
                                self.unpin_gpr(r_addr);
                                self.unpin_gpr(rs);
                            } else {
                                self.unpin_gpr(rs);
                                self.unpin_gpr(r_offset);
                                let reg_id = self.assign_reg(node.id, res_size, code_printer);
                                self.pointer_offset(reg_id, rs, r_offset, code_printer); // take care not to clobber rs
                                Self::load(reg_id, reg_id, *static_offset as isize, res_size, code_printer);
                            }
                        } else {
                            self.unpin_gpr(rs);
                            let reg_id = self.assign_reg(node.id, res_size, code_printer);
                            Self::load(reg_id, rs, *static_offset as isize, res_size, code_printer);
                        }
                    }
                }
            }
            IRDAGNodeCons::Jump(target) => {
                if let IRDAGNodeCons::Label(Some(blk_id)) = target.borrow().cons {
                    code_printer.print_jump_label(&self.gen_func_block_label(func_name, blk_id)).unwrap();
                } else {
                    panic!("Invalid jump operation with target {:?}", target.borrow().cons);
                }
            }
            IRDAGNodeCons::Branch(target, cond) => {
                let rs = self.prepare_source_reg(&*cond.borrow(), code_printer);
                self.unpin_gpr(rs);
                if let IRDAGNodeCons::Label(Some(blk_id)) = target.borrow().cons {
                    code_printer.print_branch_label(&self.gen_func_block_label(func_name, blk_id), rs).unwrap();
                } else {
                    panic!("Invalid branching operation");
                }
            }
            IRDAGNodeCons::InDomReturn(ret_val) => {
                if let Some(ret_val_node) = ret_val.as_ref() {
                    let rs = self.prepare_source_reg(&*ret_val_node.borrow(), code_printer);
                    self.unpin_gpr(rs);
                    if rs != GPR_IDX_A0 {
                        self.move_reg(GPR_IDX_A0, rs, code_printer);
                    }
                }
                self.gpr_cleanup(code_printer);
                code_printer.print_jump_label(&self.gen_func_ret_label(func_name)).unwrap();
            }
            IRDAGNodeCons::InDomCall(callee, arguments) => {
                let ret_val_size = node.vtype.size();

                for (arg_idx, arg) in arguments.iter().enumerate() {
                    let arg_reg = GPR_PARAMS[arg_idx];
                    self.prepare_source_reg_specified(&*arg.borrow(), arg_reg, code_printer);
                }
                for arg_idx in 0..arguments.len() {
                    self.unpin_gpr(GPR_PARAMS[arg_idx]);
                }

                // spill everything in caller-saved regs
                for reg_id in GPR_CALLER_SAVED_LIST.iter() {
                    if !matches!(self.gpr_states[*reg_id], GPRState::Reserved) {
                        self.spill_reg_if_taken(*reg_id, code_printer);
                    }
                }
                
                // save ra
                let ra_spill_slot = self.stack_frame.find_free_spill_slot(8); // TODO: implement for capstone abi
                code_printer.print_store_to_stack(GPR_IDX_RA, self.stack_frame.spill_stack_slot_offset(ra_spill_slot)).unwrap();
                // no need to update spill slot state because we immediately load ra back
                code_printer.print_call(callee).unwrap(); // this clobbers ra et al.
                code_printer.print_load_from_stack(GPR_IDX_RA, self.stack_frame.spill_stack_slot_offset(ra_spill_slot)).unwrap();
                
                assert!(matches!(self.gpr_states[GPR_IDX_A0], GPRState::Free));
                self.gpr_states[GPR_IDX_A0] = GPRState::Taken(node.id, ret_val_size);
                self.temps.get_mut(&node.id).unwrap().loc = TempLocation::GPR(GPR_IDX_A0);
            }
            IRDAGNodeCons::Intrinsic(intrinsic, arguments) => {
                let res_size = node.vtype.size();
                match intrinsic {
                    IntrinsicFunction::Mrev => {
                        let rs = self.prepare_source_reg(&*arguments[0].to_word().unwrap().borrow(), code_printer);
                        let reg_id = self.assign_reg(node.id, res_size, code_printer);
                        self.unpin_gpr(rs);
                        code_printer.print_mrev(reg_id, rs).unwrap();
                    }
                    IntrinsicFunction::Revoke | IntrinsicFunction::Seal | IntrinsicFunction::Delin => {
                        // destruct the parameter
                        let arg_ref = arguments[0].to_word().unwrap().borrow();
                        let rs = self.prepare_source_reg(&*arg_ref, code_printer);
                        self.destruct_temp_value(rs, &*arg_ref);
                        drop(arg_ref);

                        let reg_id = self.assign_reg_with_hint(node.id, res_size, rs, code_printer);
                        match intrinsic {
                            IntrinsicFunction::Revoke => {
                                if rs != reg_id {
                                    code_printer.print_movc(reg_id, rs).unwrap();
                                }
                                code_printer.print_revoke(reg_id).unwrap();
                            }
                            IntrinsicFunction::Delin => {
                                if rs != reg_id {
                                    code_printer.print_movc(reg_id, rs).unwrap();
                                }
                                code_printer.print_delin(reg_id).unwrap();
                            }
                            IntrinsicFunction::Seal => {
                                code_printer.print_seal(reg_id, rs).unwrap();
                            }
                            _ => panic!("Should not reach here")
                        }
                    }
                    IntrinsicFunction::Tighten => {
                        let arg_ref = arguments[0].to_word().unwrap().borrow();
                        let rs = self.prepare_source_reg(&*arg_ref, code_printer);
                        let rs_destructed = arg_ref.vtype.is_linear();
                        if rs_destructed {
                            self.destruct_temp_value(rs, &*arg_ref);
                        } else {
                            self.unpin_gpr(rs);
                        }
                        drop(arg_ref);

                        let arg_ref = arguments[1].to_word().unwrap().borrow();
                        if let IRDAGNodeCons::IntConst(perms) = &arg_ref.cons {
                            let rd = self.assign_reg_with_hint(node.id, res_size, rs, code_printer);
                            code_printer.print_tighten(rd, rs, *perms).unwrap();
                        } else {
                            panic!("Second parameter of tighten should be a constant literal");
                        }
                    }
                    IntrinsicFunction::DomCall | IntrinsicFunction::DomCallSaveS => {
                        let args_count = arguments.len() - 1;
                        for (arg_idx, arg) in arguments[1..].iter().enumerate() {
                            self.prepare_source_reg_specified(&*arg.to_word().unwrap().borrow(), 
                                GPR_PARAMS[arg_idx], code_printer);
                        }

                        let dom_ref = arguments[0].to_word().unwrap().borrow();
                        let r_dom = self.prepare_source_reg(&*dom_ref, code_printer);
                        self.destruct_temp_value(r_dom, &*dom_ref);
                        drop(dom_ref);

                        for (arg_idx, arg) in arguments[1..].iter().enumerate() {
                            self.destruct_temp_value(GPR_PARAMS[arg_idx], &*arg.to_word().unwrap().borrow());
                        }

                        let reg_id = self.assign_reg_with_hint(node.id, res_size, r_dom, code_printer);
                        // to hold the renewed domain

                        self.gpr_all_clobbered = true;
                        self.synchronous_save_context(args_count, &[reg_id], &[r_dom], 0,
                            matches!(intrinsic, IntrinsicFunction::DomCallSaveS), false, code_printer);
                        code_printer.print_domcall(reg_id, r_dom).unwrap();
                        self.synchronous_restore_context(args_count, &[reg_id], &[r_dom], 0,
                        matches!(intrinsic, IntrinsicFunction::DomCallSaveS), false, code_printer);
                    }
                    IntrinsicFunction::IHDomCall | IntrinsicFunction::IHDomCallSaveS => {
                        let args_count = arguments.len();
                        for (arg_idx, arg) in arguments.iter().enumerate() {
                            self.prepare_source_reg_specified(&*arg.to_word().unwrap().borrow(), 
                                GPR_PARAMS[arg_idx], code_printer);
                        }

                        for (arg_idx, arg) in arguments.iter().enumerate() {
                            self.destruct_temp_value(GPR_PARAMS[arg_idx], &*arg.to_word().unwrap().borrow());
                        }

                        self.gpr_all_clobbered = true;
                        self.synchronous_save_context(args_count, &[], &[], 0,
                            matches!(intrinsic, IntrinsicFunction::IHDomCallSaveS), false, code_printer);
                        code_printer.print_domcall(GPR_IDX_X0, GPR_IDX_X0).unwrap();
                        self.synchronous_restore_context(args_count, &[], &[], 0,
                        matches!(intrinsic, IntrinsicFunction::IHDomCallSaveS), false, code_printer);
                    }
                    IntrinsicFunction::DomReturn | IntrinsicFunction::DomReturnSaveS => {
                        // TODO: add separate synchronous and asynchronous versions
                        let ret_dom_ref = arguments[0].to_word().unwrap().borrow();
                        let is_async = matches!(ret_dom_ref.vtype, IRDAGNodeVType::DomAsync);
                        let r_ret_dom = self.prepare_source_reg(&*ret_dom_ref, code_printer);
                        let rs2 = self.prepare_source_reg(&*arguments[1].to_word().unwrap().borrow(), code_printer);

                        if is_async {
                            let rs3 = self.prepare_source_reg(&*arguments[2].to_word().unwrap().borrow(), code_printer);
                            self.destruct_temp_value(r_ret_dom, &*ret_dom_ref);
                            drop(ret_dom_ref);
                            self.unpin_gpr(rs2);
                            self.unpin_gpr(rs3);

                            code_printer.print_domreturn(r_ret_dom, rs2, rs3).unwrap();
                        } else {
                            let smode_dom = matches!(intrinsic, IntrinsicFunction::DomReturnSaveS);
                            self.destruct_temp_value(r_ret_dom, &*ret_dom_ref);
                            drop(ret_dom_ref);
                            self.unpin_gpr(rs2);
                            self.domreturn_save_context(r_ret_dom, rs2, if smode_dom { SMODE_CONTEXT_SIZE } else { 0 }, smode_dom, code_printer);
                            
                            code_printer.print_domreturn(r_ret_dom, rs2, GPR_IDX_X0).unwrap();
                        }
                    }
                    IntrinsicFunction::Capfield => {
                        let arg_ref = arguments[0].to_word().unwrap().borrow();
                        let rs1 = self.prepare_source_reg(&*arg_ref, code_printer);
                        let rd = if !arg_ref.vtype.is_linear() {
                            self.unpin_gpr(rs1);
                            self.assign_reg_with_hint(node.id, 8, rs1, code_printer)
                        } else {
                            let rd = self.assign_reg(node.id, 8, code_printer);
                            self.unpin_gpr(rs1);
                            rd
                        };
                        if let IRDAGNodeCons::IntConst(imm) = &arguments[1].to_word().unwrap().borrow().cons {
                            code_printer.print_lcc(rd, rs1, *imm).unwrap();
                        } else {
                            panic!("Capfield arg1 must be a const literal");
                        }
                    }
                    IntrinsicFunction::Split => {
                        // let rs1_lval = self.argument
                        let a1_lval = arguments[0].to_lval().unwrap();
                        let a1_word = arguments[2].to_word().unwrap();
                        let a2_word = arguments[1].to_word().unwrap();

                        let rs1 = self.prepare_source_reg(&*a1_word.borrow(), code_printer);
                        let rs2 = self.prepare_source_reg(&*a2_word.borrow(), code_printer);

                        self.unpin_gpr(rs2);
                        let rd = self.assign_reg_with_hint(node.id, self.globals.target_conf.register_width,
                            rs2, code_printer);

                        // TODO: changing rs1 directly would be problematic if the original data is still needed
                        // assert_eq!(self.temps.get(&a1_word.borrow().id).unwrap().rev_deps_to_eval, 0);
                        code_printer.print_split(rd, rs1, rs2).unwrap();
                    
                        self.gen_writeback_to_loc(&a1_lval.loc, rs1, 
                            self.globals.target_conf.register_width, false, code_printer);
                    }
                }
            }
            IRDAGNodeCons::Asm(asm, outputs, inputs) => {
                eprintln!("asm with {} inputs and {} outputs", inputs.len(), outputs.len());
                let output_regs = Vec::from_iter(outputs.iter().map(
                    |out| {
                        match &out.constraint {
                            IRDAGAsmOutputConstraint::Overwrite => {
                                let res = self.assign_reg(ANON_IRDAG_NODE_ID, out.size, code_printer);
                                self.pin_gpr(res);
                                res
                            }
                            IRDAGAsmOutputConstraint::ReadWrite => {
                                panic!("ReadWrite asm output unsupported");
                            }
                        }
                    }
                ));
                let input_regs = Vec::from_iter(inputs.iter().map(
                    |inp| {
                        self.prepare_source_reg(&*inp.value.borrow(), code_printer)
                    }
                ));
                // unpin inputs only
                for reg in input_regs.iter() {
                    self.unpin_gpr(*reg);
                }
                // now simply do pattern replacement
                let mut asm_res = asm.clone();
                for (reg_idx, (symb_name_op, reg)) in 
                    outputs.iter().map(|out| &out.symb_name).chain(
                        inputs.iter().map(|inp| &inp.symb_name)
                    ).zip(output_regs.iter().chain(input_regs.iter())).enumerate() {
                    asm_res = asm_res.replace(&format!("%{}", reg_idx), REG_NAMES[*reg]);
                    if let Some(symb_name) = symb_name_op {
                        asm_res = asm_res.replace(&format!("[{}]", symb_name), REG_NAMES[*reg]);
                    }
                }
                code_printer.print_asm(&asm_res).unwrap();
                // write back result to output locations
                for (reg, out) in output_regs.iter().zip(outputs.iter()) {
                    if let Some(named) = out.loc.get_named_mem_loc() {
                        self.discard_var_state(*self.vars_to_ids.get(named).unwrap());
                    }
                    self.gen_writeback_to_loc(&out.loc, *reg, out.size, true, code_printer);
                    self.unpin_gpr(*reg);
                }

                // clean up as this is end of basic block
                self.gpr_cleanup(code_printer);
            }
            IRDAGNodeCons::CapResize(src, size) => {
                let rs = self.prepare_source_reg(&*src.borrow(), code_printer);
                let rd = if src.borrow().vtype.is_linear() {
                    let rd = self.assign_reg(node.id, src.borrow().vtype.size(), code_printer);
                    self.unpin_gpr(rs);
                    rd
                } else {
                    self.unpin_gpr(rs);
                    self.assign_reg_with_hint(node.id, src.borrow().vtype.size(), rs, code_printer)
                };
                self.pointer_bound_imm(rd, rs, *size, code_printer);
            }
            IRDAGNodeCons::LocalSymbol(symbol_name) => {
                let rd = self.assign_reg(node.id, 8, code_printer);
                code_printer.print_lla(rd, &symbol_name).unwrap();
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
            if !self.temps.contains_key(&node_ref.id) {
                self.temps.insert(node_ref.id, TempState {
                    var: None,
                    loc: TempLocation::Nowhere,
                    rev_deps_to_eval: node_ref.rev_deps.len()
                });
            }
        }
    }

    // reset for starting labeled basic block
    fn codegen_block_labeled_reset<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // self.temps.clear();
        self.gpr_states.fill(GPRState::Free);
        // reserve some special registers
        for reserved_gpr in GPR_RESERVED_LIST {
            self.gpr_states[reserved_gpr] = GPRState::Reserved;
        }
        
        self.stack_frame.clear_spill_slots();

        self.codegen_block_unlabeled_reset(func_name, block, code_printer);

        for var_info in self.vars.iter() {
            assert!(!var_info.state.dirty);
            assert!(matches!(var_info.state.loc, VarLocation::StackSlot));
        }
    }

    fn codegen_block_reset<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        if block.labeled {
            self.codegen_block_labeled_reset(func_name, block, code_printer);
        } else {
            self.codegen_block_unlabeled_reset(func_name, block, code_printer);
        }
    }

    fn gpr_cleanup<T>(&mut self, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        for var_info in self.vars.iter_mut() {
            match var_info.state.loc {
                VarLocation::GPR(reg_id) => {
                    if var_info.state.dirty {
                        let stack_slot = var_info.state.stack_slot;
                        Self::store(reg_id, GPR_IDX_SP, self.stack_frame.stack_slot_offset(stack_slot) as isize,
                            var_info.ty.size(&self.globals.target_conf), code_printer);
                        var_info.state.dirty = false;
                    }
                    if let GPRState::Taken(node_id, _) = self.gpr_states[reg_id] {
                        self.temps.get_mut(&node_id).unwrap().loc = TempLocation::Nowhere;
                    }
                    var_info.state.loc = VarLocation::StackSlot;
                    self.gpr_states[reg_id] = GPRState::Free;
                },
                VarLocation::StackSlot => {
                    assert!(!var_info.state.dirty);
                }
            }
        }
    }

    fn codegen_block_end_cleanup<T>(&mut self, func_name: &str, block: &IRDAGBlock, code_printer: &mut CodePrinter<T>) where T: std::io::Write {
        // write back all dirty variables
        let (need_cleanup, deps) = match block.exit_node.as_ref() {
            Some(exit_node) => {
                let deps = Vec::from_iter(exit_node.borrow().deps.iter().cloned());
                if matches!(&exit_node.borrow().cons, IRDAGNodeCons::InDomReturn(_)) {
                    (false, deps)
                } else {
                    (true, deps)
                }
            }
            _ => (true, Vec::new())
        };
        // Non-dependencies: always clean up
        // Dependencies:
        // - Nonlinear: write back dirty values, but leave results in register
        // - Linear: do nothing. The node implementation is responsible for any writeback if necessary
        if need_cleanup {
            self.gpr_cleanup(code_printer);
        }
        if let Some(exit_node) = block.exit_node.as_ref() {
            let mut exit_node_ref = exit_node.borrow_mut();
            self.generate_node(func_name, &*&exit_node_ref, code_printer);
            while let Some(rev_dep) = exit_node_ref.rev_deps.pop() {
                let mut rev_dep_m = rev_dep.borrow_mut();
                rev_dep_m.dep_count -= 1;
            }
        }
    }

    // this consumes codegen itself as codegen can only happen once
    fn codegen<Out>(mut self, func: &CaplanFunction, ctx: &GlobalCodeGenContext, out: &mut Out) where Out: std::io::Write {
        eprintln!("Codegen for function {}", func.name);

        let mut prologue_code_printer: CodePrinter<Vec<u8>> = CodePrinter::new(Vec::new(), self.globals.target_conf.clone());
        let mut main_code_printer : CodePrinter<Vec<u8>> = CodePrinter::new(Vec::new(), self.globals.target_conf.clone());

        // for all variables that ever appear, allocate one stack slot
        // we handle parameters in the same way
        for param in func.params.iter() {
            let var_id = self.vars.len();
            let mem_loc = IRDAGNamedMemLoc {
                var_name: param.name.clone(),
                offset: 0
            }; // TODO: param's can only be 8 bytes
            self.vars_to_ids.insert(mem_loc, var_id);
            // TODO: passing params through stack is not supported
            eprintln!("Allocated slot {} to param {}", self.stack_frame.stack_slots.len(), param.name);
            let param_size = param.ty.size(&self.globals.target_conf);
            assert!(param_size == 8 || param_size == 16, "Each parameter must fit in a register");
            let stack_slot = self.stack_frame.allocate_stack_slot(param_size);
            self.vars.push(VarInfo {
                state: VarState {
                    loc: VarLocation::StackSlot,
                    stack_slot: stack_slot,
                    dirty: false
                },
                ty: param.ty.clone()
            });
        }

        for (local_name, local_type) in func.locals.iter() {
            let align_log = if local_type.alignment(&self.globals.target_conf) == 8 { 3 } else { 4 };
            let base_offset = align_up_to(self.stack_frame.tot_stack_slot_size, align_log);
            local_type.visit_offset(&mut|offset, ty| {
                let mem_loc = IRDAGNamedMemLoc {
                    var_name: local_name.clone(),
                    offset: offset
                };
                let var_id = self.vars.len();
                self.vars_to_ids.insert(mem_loc, var_id);
                let stack_slot = self.stack_frame.allocate_stack_slot_specified_offset(offset + base_offset, ty.size(&self.globals.target_conf));
                self.vars.push(VarInfo {
                    state: VarState {
                        loc: VarLocation::StackSlot,
                        stack_slot: stack_slot,
                        dirty: false
                    },
                    ty: ty
                });
               
            }, 0, &self.globals.target_conf);
        }

        let mut stack_bottom : Vec<GCed<IRDAGNode>> = Vec::new();
        // for each basic block, do a topo sort
        for (blk_id, block) in func.dag.blocks.iter().enumerate() {
            if block.is_empty() {
                continue;
            }
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
                let mut node_ref = node.borrow_mut();
                self.generate_node(&func.name, &*node_ref, &mut main_code_printer);
                while let Some(rev_dep) = node_ref.rev_deps.pop() {
                    let mut rev_dep_m = rev_dep.borrow_mut();
                    rev_dep_m.dep_count -= 1;
                    if rev_dep_m.dep_count == 0 && !rev_dep_m.cons.is_control_flow() {
                        if rev_dep_m.destructs.is_empty() {
                            self.op_topo_stack.push(rev_dep.clone());
                        } else {
                            stack_bottom.push(rev_dep.clone());
                        }
                    }
                }
                if self.op_topo_stack.is_empty() {
                    self.op_topo_stack.append(&mut stack_bottom);
                }
            }
            // check that all nodes have dep_count = 0
            for node in block.dag.iter() {
                let node_ref = node.borrow();
                assert!(node_ref.dep_count == 0, "Node still not processed = {}{:?} (dep = {})",
                    node_ref.id, node_ref.cons, node_ref.dep_count);
            }
            self.codegen_block_end_cleanup(&func.name, block, &mut main_code_printer);
        }
        
        self.generate_prologue(&func, ctx, &mut prologue_code_printer);
        self.generate_epilogue(&func, ctx, &mut main_code_printer);

        out.write_all(&prologue_code_printer.get_out()).unwrap();
        out.write_all(&main_code_printer.get_out()).unwrap();
        writeln!(out).unwrap();
    }
}

pub struct CodeGen<Out> where Out: std::io::Write {
    ctx: GlobalCodeGenContext,
    code_printer: CodePrinter<Out>
}

impl<Out> CodeGen<Out> where Out: std::io::Write {
    pub fn new(translation_unit: CaplanTranslationUnit, out: Out) -> Self {
        let target_conf = translation_unit.globals.target_conf.clone();
        Self {
            ctx: GlobalCodeGenContext::new(translation_unit),
            code_printer: CodePrinter::new(out, target_conf)
        }
    }

    pub fn codegen(mut self) {
        self.code_printer.print_defs().unwrap();
        writeln!(&mut self.code_printer.out).unwrap();

        for func in self.ctx.translation_unit.functions.iter() {
            let func_codegen = FunctionCodeGen::new(&self.ctx.translation_unit.globals);
            func_codegen.codegen(func, &self.ctx, &mut self.code_printer.out);
        }

        match &self.ctx.translation_unit.globals.target_conf.abi {
            CaplanABI::RISCV64 => {
                // bss declaration
                for (var_name, var_type) in self.ctx.translation_unit.globals.global_vars.iter() {
                    // TODO: throw the io error out
                    // writeln!(&mut self.code_printer.out, ".align {}", alignment_bits).unwrap();
                    writeln!(&mut self.code_printer.out, ".comm {}, {}, {}",
                        var_name,
                        var_type.size(&self.ctx.translation_unit.globals.target_conf),
                        var_type.alignment(&self.ctx.translation_unit.globals.target_conf)).unwrap();
                }
            }
            CaplanABI::CapstoneCGNLSD => {
                // TODO: produce the caplocation table in a special table
                self.code_printer.print_align(16).unwrap();
                self.code_printer.print_section(".gct").unwrap();
                for (_, var_type) in self.ctx.translation_unit.globals.global_vars.iter() {
                    self.code_printer.print_u64(var_type.size(&self.ctx.translation_unit.globals.target_conf) as u64).unwrap();
                    self.code_printer.print_u64(0x6).unwrap();
                }
            }
        }
    }
}

