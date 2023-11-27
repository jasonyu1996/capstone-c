use crate::lang_defs::CaplanType;

#[derive(Clone, Debug)]
pub enum CaplanABI {
    RISCV64,
    CAPSTONE_CG_NL_SD
}

#[derive(Clone, Debug)]
pub struct CaplanTargetConf {
    pub abi: CaplanABI,
    // width of each register in bytes
    pub register_width: usize,
    pub min_alignment: usize // TODO: suboptimal
}

impl CaplanTargetConf {
    pub fn new(abi: CaplanABI) -> Self {
        let clen = match &abi {
            CaplanABI::RISCV64 => 8,
            CaplanABI::CAPSTONE_CG_NL_SD => 16
        };
        Self {
            abi: abi,
            register_width: clen,
            min_alignment: clen
        }
    }

    pub fn make_pointer(&self, ty: &mut CaplanType) {
        let t = std::mem::replace(ty, CaplanType::Void);
        *ty = match &self.abi {
            CaplanABI::RISCV64 => CaplanType::RawPtr(Box::new(t)),
            CaplanABI::CAPSTONE_CG_NL_SD => CaplanType::NonlinPtr(Box::new(t))
        };
    }
}
