use super::arch_defs::*;

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
const STACK_SLOT_SIZE : usize = 8;

pub fn print_li(rd: RegId, v: u64) {
    println!("{}li {}, {}", INST_INDENT, REG_NAMES[rd], v);
}

pub fn print_load_from_stack_slot(rd: RegId, slot: usize) {
    println!("{}ld {}, {}(sp)", INST_INDENT, REG_NAMES[rd], STACK_SLOT_SIZE * slot);
}

pub fn print_store_to_stack_slot(rs: RegId, slot: usize) {
    println!("{}sd {}, {}(sp)", INST_INDENT, REG_NAMES[rs], STACK_SLOT_SIZE * slot);
}

pub fn print_add(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}add {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_sub(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_mul(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}mul {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_div(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}div {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_and(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}and {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_or(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}or {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_xor(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}xjkor {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_eq(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
    println!("{}sltu {}, x0, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd]);
    println!("{}xori {}, {}, 1", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd]);
}

pub fn print_neq(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
    println!("{}sltu {}, x0, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd]);
}

pub fn print_lt(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}sltu {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs1], REG_NAMES[rs1]);
}

pub fn print_le(rd: RegId, rs1: RegId, rs2: RegId) {
    println!("{}sub {}, {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs2], REG_NAMES[rs1]);
    println!("{}addi {}, {}, 1", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd]);
    println!("{}slt {}, x0, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rd]);
}

pub fn print_mv(rd: RegId, rs: RegId) {
    println!("{}mv {}, {}", INST_INDENT, REG_NAMES[rd], REG_NAMES[rs]);
}

pub fn print_label(name: &str) {
    println!("{}:", name);
}

pub fn print_jump_label(name: &str) {
    println!("{}j {}", INST_INDENT, name);
}

pub fn print_branch_label(name: &str, rs: RegId) {
    println!("{}bnez {}, {}", INST_INDENT, REG_NAMES[rs], name);
}
