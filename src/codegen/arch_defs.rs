use std::slice::Iter;

pub const GPR_N : usize = 32;

pub type RegId = usize;
pub const GPR_IDX_X0 : RegId = 0;
pub const GPR_IDX_RA : RegId = 1;
pub const GPR_IDX_SP : RegId = 2;
pub const GPR_IDX_GP : RegId = 3;
pub const GPR_IDX_TP : RegId = 4;
pub const GPR_IDX_A0 : RegId = 10;
pub const GPR_RESERVED_LIST : [RegId; 5] = [GPR_IDX_X0, GPR_IDX_RA, GPR_IDX_SP, GPR_IDX_GP, GPR_IDX_TP];
pub const GPR_CALLEE_SAVED_LIST_BELOW16 : [RegId; 6] = [2, 6, 7, 8, 9, 14];
pub const GPR_CALLER_SAVED_LIST : [RegId; 9] = [1, 3, 4, 5, 10, 11, 12, 13, 15];


pub struct GPRCalleeSavedIter {
    below16_iter: Iter<'static, RegId>,
    above16_cursor: RegId
}

impl GPRCalleeSavedIter {
    pub fn new() -> Self {
        Self {
            below16_iter: GPR_CALLEE_SAVED_LIST_BELOW16.iter(),
            above16_cursor: 16
        }
    }
}

impl Iterator for GPRCalleeSavedIter {
    type Item = RegId;
    fn next(&mut self) -> Option<Self::Item> {
        self.below16_iter.next().copied().or_else(
            || {
                if self.above16_cursor < GPR_N {
                    self.above16_cursor += 1;
                    Some(self.above16_cursor - 1)
                } else {
                    None
                }
            }
        )
    }
}

pub struct GPRCallerSavedIter(Iter<'static, RegId>);

impl GPRCallerSavedIter {
    pub fn new() -> Self {
        Self(GPR_CALLER_SAVED_LIST.iter())
    }
}

impl Iterator for GPRCallerSavedIter {
    type Item = RegId;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().copied()
    }
}
