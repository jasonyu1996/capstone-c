pub const GPR_N : usize = 32;

pub type RegId = usize;
pub const GPR_IDX_X0 : RegId = 0;
pub const GPR_IDX_RA : RegId = 1;
pub const GPR_IDX_SP : RegId = 2;
pub const GPR_IDX_GP : RegId = 3;
pub const GPR_IDX_TP : RegId = 4;
pub const GPR_IDX_T0 : RegId = 5;
pub const GPR_IDX_T1 : RegId = 6;
pub const GPR_IDX_T2 : RegId = 7;
pub const GPR_IDX_A0 : RegId = 10;
pub const GPR_IDX_A1 : RegId = 11;
pub const GPR_IDX_A2 : RegId = 12;
pub const GPR_IDX_A3 : RegId = 13;
pub const GPR_IDX_A4 : RegId = 14;
pub const GPR_IDX_A5 : RegId = 15;
pub const GPR_RESERVED_LIST : [RegId; 5] = [GPR_IDX_X0, GPR_IDX_RA, GPR_IDX_SP, GPR_IDX_GP, GPR_IDX_TP];
pub const GPR_CALLEE_SAVED_LIST : [RegId; 13] = [2, 8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27];
pub const GPR_CALLER_SAVED_LIST : [RegId; 16] = [1, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 28, 29, 30, 31];
pub const GPR_PARAMS : [RegId; 8] = [10, 11, 12, 13, 14, 15, 16, 17];

pub const DOM_SAVE_GPR : &'static [RegId] = &[GPR_IDX_RA, GPR_IDX_GP];
pub const DOM_SAVE_GPR_DOM_BOUNDARY : &'static [RegId] = &[GPR_IDX_GP];
pub const DOM_SAVE_CCSR : &'static [&'static str] = &["cscratch"];
pub const DOM_SAVE_CCSR_SMODE : &'static [&'static str] = &["cepc", "cmmu"];
pub const DOM_SAVE_CSR : &'static [&'static str] = &["mcause", "mtval"]; // "mtinst"];
pub const DOM_SAVE_CSR_SMODE : &'static [&'static str] = &["stvec", "scause", "stval",
    "sepc", "sscratch", "satp", "offsetmmu"];

pub fn get_dom_save_context(save_s: bool) -> (Box<dyn Iterator<Item=&'static &'static str>>, Box<dyn Iterator<Item=&'static &'static str>>) {
    if save_s {
        (
            Box::new(DOM_SAVE_CCSR.iter().chain(DOM_SAVE_CCSR_SMODE.iter())),
            Box::new(DOM_SAVE_CSR.iter().chain(DOM_SAVE_CSR_SMODE.iter())),
        )
    } else {
        (Box::new(DOM_SAVE_CCSR.iter()), Box::new(DOM_SAVE_CSR.iter()))
    }
}


pub fn get_save_gpr_iter(dom_boundary: bool) -> std::slice::Iter<'static, RegId> {
    if dom_boundary {
        DOM_SAVE_GPR_DOM_BOUNDARY
    } else {
        DOM_SAVE_GPR
    }.iter()
}

#[repr(u64)]
pub enum LccField {
    Valid = 0,
    Type,
    Cursor,
    Base,
    End,
    Perms,
    Async,
    Reg
}
