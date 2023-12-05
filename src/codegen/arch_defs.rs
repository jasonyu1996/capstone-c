pub const GPR_N : usize = 32;

pub type RegId = usize;
pub const GPR_IDX_X0 : RegId = 0;
pub const GPR_IDX_RA : RegId = 1;
pub const GPR_IDX_SP : RegId = 2;
pub const GPR_IDX_GP : RegId = 3;
pub const GPR_IDX_TP : RegId = 4;
pub const GPR_IDX_T0 : RegId = 5;
pub const GPR_IDX_A0 : RegId = 10;
pub const GPR_RESERVED_LIST : [RegId; 5] = [GPR_IDX_X0, GPR_IDX_RA, GPR_IDX_SP, GPR_IDX_GP, GPR_IDX_TP];
pub const GPR_CALLEE_SAVED_LIST : [RegId; 13] = [2, 8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27];
pub const GPR_CALLER_SAVED_LIST : [RegId; 16] = [1, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 28, 29, 30, 31];
pub const GPR_PARAMS : [RegId; 8] = [10, 11, 12, 13, 14, 15, 16, 17];

pub const CCSR_DOMAIN_SAVED : &'static [&'static str] = &[
    // "cscratch" // TODO: MORE
];
