use crate::target_conf::{CaplanTargetConf, self, CaplanABI};

use super::arch_defs::*;
use std::io::Write;

pub const REG_NAMES : [&'static str; GPR_N] = [
    "x0", "ra", "sp", "gp", "tp",
    "t0", "t1", "t2", "s0", "s1",
    "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3",
    "s4", "s5", "s6", "s7", "s8",
    "s9", "s10", "s11", "t3", "t4",
    "t5", "t6"
];

const INST_INDENT : &'static str = "  ";

// defines
const ASM_DEFS_CAPSTONE : &'static [(&'static str, &'static str)] = &[
    ("cincoffset(rd, rs1, rs2)", ".insn r 0x5b, 0x1, 0xc, rd, rs1, rs2"),
    ("cincoffsetimm(rd, rs, offset)", ".insn i 0x5b, 0x2, rd, offset(rs)"),
    ("movc(rd, rs)", ".insn r 0x5b, 0x1, 0xa, rd, rs, x0"),
    ("ldc(rd, rs, offset)", ".insn i 0x5b, 0x3, rd, offset(rs)"),
    ("stc(rs1, rs2, offset)", ".insn s 0x5b, 0x4, rs1, offset(rs2)"),
    ("shrinkto(rd, rs, size)", ".insn i 0x5b, 0x0, rd, size(rs)"),
    ("mrev(rd, rs)", ".insn r 0x5b, 0x1, 0x8, rd, rs, x0"),
    ("revoke(rs)", ".insn r 0x5b, 0x1, 0x0, x0, rs, x0"),
    ("seal(rd, rs)", ".insn r 0x5b, 0x1, 0x7, rd, rs, x0"),
    ("domcall(rd, rs)", ".insn r 0x5b, 0x1, 0x20, rd, rs, x0"),
    ("domreturn(rd, rs1, rs2)", ".insn r 0x5b, 0x1, 0x21, rd, rs1, rs2")
];

pub struct CodePrinter<T> where T: Write {
    pub out: T,
    target_conf: CaplanTargetConf
}

impl<T> CodePrinter<T> where T: Write {
    pub fn new(out: T, target_conf: CaplanTargetConf) -> Self {
        Self {
            out: out,
            target_conf: target_conf
        }
    }

    pub fn print_defs(&mut self) -> Result<(), std::io::Error> {
        if matches!(self.target_conf.abi, CaplanABI::CapstoneCGNLSD) {
            for (macro_name, macro_def) in ASM_DEFS_CAPSTONE.iter() {
                writeln!(&mut self.out, "#define {} {}", *macro_name, *macro_def)?;
            }
        }
        Ok(())
    }

    pub fn print_la(&mut self, rd: RegId, symbol: &str) ->  Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}la {}, {}", INST_INDENT, REG_NAMES[rd], symbol)?;
        Ok(())
    }

    pub fn print_li(&mut self, rd: RegId, v: u64) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}li {}, {}", INST_INDENT, REG_NAMES[rd], v)?;
        Ok(())
    }

    pub fn print_load_from_stack(&mut self, rd: RegId, offset: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}ld {}, {}(sp)", INST_INDENT, REG_NAMES[rd], offset)?;
        Ok(())
    }

    pub fn print_store_to_stack(&mut self, rs: RegId, offset: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sd {}, {}(sp)", INST_INDENT, REG_NAMES[rs], offset)?;
        Ok(())
    }

    pub fn print_ld(&mut self, rd: RegId, rs: RegId, offset: isize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}ld {}, {}({})", INST_INDENT, REG_NAMES[rd], offset, REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_sd(&mut self, rs1: RegId, rs2: RegId, offset: isize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sd {}, {}({})", INST_INDENT, REG_NAMES[rs1], offset, REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_addi(&mut self, rd: RegId, rs1: RegId, offset: isize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}addi {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], offset)?;
        Ok(())
    }

    pub fn print_add(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}add {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_sub(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_mul(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}mul {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_div(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}div {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_and(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}and {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_or(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}or {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_xor(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}xor {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_eq(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        writeln!(&mut self.out, "{}sltu {}, x0, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd])?;
        writeln!(&mut self.out, "{}xori {}, {}, 1", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd])?;
        Ok(())
    }

    pub fn print_neq(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        writeln!(&mut self.out, "{}sltu {}, x0, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd])?;
        Ok(())
    }

    pub fn print_lt(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sltu {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_le(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs2], REG_NAMES[rs1])?;
        writeln!(&mut self.out, "{}addi {}, {}, 1", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd])?;
        writeln!(&mut self.out, "{}slt {}, x0, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd])?;
        Ok(())
    }

    pub fn print_mv(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}mv {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_neg(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}neg {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_not(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}not {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_label(&mut self, name: &str) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}:", name)?;
        Ok(())
    }

    pub fn print_jump_label(&mut self, name: &str) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}j {}", INST_INDENT, name)?;
        Ok(())
    }

    pub fn print_branch_label(&mut self, name: &str, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}bnez {}, {}", INST_INDENT, REG_NAMES[rs], name)?;
        Ok(())
    }
    
    pub fn print_call(&mut self, name: &str) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}call {}", INST_INDENT, name)?;
        Ok(())
    }

    pub fn print_global_symb(&mut self, name: &str) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, ".global {}", name)?;
        Ok(())
    }

    pub fn print_ret(&mut self) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}ret", INST_INDENT)?;
        Ok(())
    }

    pub fn print_asm(&mut self, asm: &str) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}{}", INST_INDENT, asm)?;
        Ok(())
    }

    pub fn print_align(&mut self, align: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, ".align {}", align)?;
        Ok(())
    }

    pub fn print_section(&mut self, sec_name: &str) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, ".section {}", sec_name)?;
        Ok(())
    }

    pub fn print_u64(&mut self, mut val: u64) -> Result<(), std::io::Error> {
        write!(&mut self.out, "{}", INST_INDENT)?;
        for _ in 0..8 {
            write!(&mut self.out, ".byte {:#04x}; ", val & 255)?;
            val >>= 8;
        }
        writeln!(&mut self.out)?;
        Ok(())
    }

    pub fn get_out(self) -> T {
        self.out
    }

    pub fn print_sll(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> { 
        writeln!(&mut self.out, "{}sll {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_srl(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> { 
        writeln!(&mut self.out, "{}srl {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_slli(&mut self, rd: RegId, rs: RegId, imm: usize) -> Result<(), std::io::Error> { 
        writeln!(&mut self.out, "{}slli {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs], imm)?;
        Ok(())
    }

    pub fn print_srli(&mut self, rd: RegId, rs: RegId, imm: usize) -> Result<(), std::io::Error> { 
        writeln!(&mut self.out, "{}srli {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs], imm)?;
        Ok(())
    }


    /* Capstone-specific */

    pub fn print_ldc(&mut self, rd: RegId, rs: RegId, offset: isize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}ldc({}, {}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs], offset)?;
        Ok(())
    }

    pub fn print_stc(&mut self, rs1: RegId, rs2: RegId, offset: isize) -> Result<(), std::io::Error> { 
        writeln!(&mut self.out, "{}stc({}, {}, {})", INST_INDENT, REG_NAMES[rs1], REG_NAMES[rs2], offset)?;
        Ok(())
    }

    pub fn print_load_cap_from_stack(&mut self, rd: RegId, offset: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}ldc({}, sp, {})", INST_INDENT, REG_NAMES[rd], offset)?;
        Ok(())
    }

    pub fn print_store_cap_to_stack(&mut self, rs: RegId, offset: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}stc({}, sp, {})", INST_INDENT, REG_NAMES[rs], offset)?;
        Ok(())
    }

    pub fn print_incoffsetimm(&mut self, rd: RegId, rs: RegId, offset: isize) ->Result<(), std::io::Error> { 
        writeln!(&mut self.out, "{}cincoffsetimm({}, {}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs], offset)?;
        Ok(())
    }

    pub fn print_incoffset(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}cincoffset({}, {}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_movc(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}movc({}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_shrinkto(&mut self, rd: RegId, rs: RegId, size: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}shrinkto({}, {}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs], size)?;
        Ok(())
    }

    pub fn print_mrev(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}mrev({}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_revoke(&mut self, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}revoke({})", INST_INDENT, REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_seal(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}seal({}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }

    pub fn print_domreturn(&mut self, rd: RegId, rs1: RegId, rs2: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}domreturn({}, {}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs2])?;
        Ok(())
    }

    pub fn print_domcall(&mut self, rd: RegId, rs: RegId) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}domcall({}, {})", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs])?;
        Ok(())
    }
}
