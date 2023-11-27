use crate::target_conf::{CaplanTargetConf, self};

use super::arch_defs::*;
use std::io::Write;

const REG_NAMES : [&'static str; GPR_N] = [
    "x0", "ra", "sp", "gp", "tp",
    "t0", "t1", "t2", "s0", "s1",
    "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3",
    "s4", "s5", "s6", "s7", "s8",
    "s9", "s10", "s11", "t3", "t4",
    "t5", "t6"
];

const INST_INDENT : &'static str = "  ";

pub struct CodePrinter<T> where T: Write {
    out: T,
    target_conf: CaplanTargetConf
}

impl<T> CodePrinter<T> where T: Write {
    pub fn new(out: T, target_conf: CaplanTargetConf) -> Self {
        Self {
            out: out,
            target_conf: target_conf
        }
    }

    pub fn print_la(&mut self, rd: RegId, symbol: &str) ->  Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}la {}, {}", INST_INDENT, REG_NAMES[rd], symbol)?;
        Ok(())
    }

    pub fn print_li(&mut self, rd: RegId, v: u64) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}li {}, {}", INST_INDENT, REG_NAMES[rd], v)?;
        Ok(())
    }

    pub fn print_load_from_stack_slot(&mut self, rd: RegId, slot: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}ld {}, {}(sp)", INST_INDENT, REG_NAMES[rd], self.target_conf.register_width * slot)?;
        Ok(())
    }

    pub fn print_store_to_stack_slot(&mut self, rs: RegId, slot: usize) -> Result<(), std::io::Error> {
        writeln!(&mut self.out, "{}sd {}, {}(sp)", INST_INDENT, REG_NAMES[rs], self.target_conf.register_width * slot)?;
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

    pub fn get_out(self) -> T {
        self.out
    }
}
