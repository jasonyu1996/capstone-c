pub const GPR_N : usize = 32;

pub type RegId = usize;
pub const GPR_IDX_X0 : RegId = 0;
pub const GPR_IDX_RA : RegId = 1;
pub const GPR_IDX_SP : RegId = 2;
pub const GPR_RESERVED_LIST : [RegId; 3] = [GPR_IDX_X0, GPR_IDX_RA, GPR_IDX_SP];