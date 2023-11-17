/* MIT License

Copyright (c) 2023 National University of Singapore

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. */

#![feature(box_patterns, once_cell)]

use std::fmt::Display;
/**
 * Features omitted: no distinguishing different int sizes
 *
 * */


use std::collections::HashMap;
use std::str::FromStr;
use lazy_static::lazy_static;

use lang_c::driver::Error;
use lang_c::{driver::{Config, parse}, ast::{TranslationUnit, FunctionDefinition, Declarator,
    Declaration, DeclaratorKind, Statement, BlockItem, InitDeclarator, Integer, Initializer,
    Expression, Constant}}; 
use lang_c::ast::{ExternalDeclaration, BinaryOperatorExpression,
    BinaryOperator, Identifier, IfStatement, WhileStatement, CallExpression, AsmStatement,
    UnaryOperatorExpression, UnaryOperator, MemberExpression, MemberOperator, DeclarationSpecifier,
    TypeSpecifier, StructType, StructKind, StructDeclaration, StructField, SpecifierQualifier,
    DerivedDeclarator, FunctionDeclarator, ParameterDeclaration, Extension, ArraySize, TypeName};
use lang_c::span::Node;
use clap::Parser as ClapParser;

const CAPSTONE_GPR_N: usize = 31; // number of general-purpose registers
const CAPSTONE_STACK_REG: CapstoneReg = CapstoneReg::Gpr(1); // sp is used for storing the stack capability
const CAPSTONE_RA_REG: CapstoneReg = CapstoneReg::Gpr(0); // ra is used for return address
const CAPSTONE_SEALED_REGION_SIZE: usize = CAPSTONE_GPR_N + 4;

const CAPSTONE_SEALED_OFFSET_PC: usize = 0;

const CAPSTONE_GPR_NAMES : [&'static str; CAPSTONE_GPR_N + 1] = [
    "zero", "ra", "sp", "gp", "tp",
    "t0", "t1", "t2",
    "s0", "s1",
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
    "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
    "t3", "t4", "t5", "t6"
];

type CapstoneInt = u64;

#[derive(ClapParser)]
#[clap(author, version, about)]
struct Args {
    source: String,
    #[clap(long, default_value_t = 500)]
    clock_rate: u32,
    #[clap(long, default_value_t = 1<<14)]
    stack_size: u32,
    #[clap(long, default_value_t = 1<<16)]
    mem_size: usize,
    #[clap(long, default_value_t = 32)]
    gpr_n: usize,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    print_comments: bool
}

lazy_static! {
    static ref CLI_ARGS : Args = Args::parse();
}


#[derive(Clone, Copy, Debug)]
enum CapstoneRegState {
    Pinned,
    Grabbed,
    Free
}

impl CapstoneRegState {
    fn grab(&mut self) {
        if !matches!(*self, CapstoneRegState::Pinned) {
            *self = CapstoneRegState::Grabbed;
        }
    }

    fn free(&mut self) {
        if !matches!(*self, CapstoneRegState::Pinned) {
            *self = CapstoneRegState::Free;
        }
    }

    fn pin(&mut self) {
        *self = CapstoneRegState::Pinned;
    }

    fn reset(&mut self) {
        *self = CapstoneRegState::Free;
    }

    fn is_available(&self) -> bool {
        matches!(*self, CapstoneRegState::Free)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum CapstoneReg {
    Pc, Epc, Ret, Gpr(u8)
}

impl Display for CapstoneReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
           CapstoneReg::Pc => {
               write!(f, "pc")
           }
           CapstoneReg::Epc => {
               write!(f, "epc")
           }
           CapstoneReg::Ret => {
               write!(f, "ret")
           }
           CapstoneReg::Gpr(n) => {
               write!(f, "{}", CAPSTONE_GPR_NAMES[*n as usize + 1])
           }
        }
    }
}

#[derive(Clone, Debug)]
enum CapstoneImm {
    NTag(CapstoneNTag, bool), // bool: relative
    STag(CapstoneSTag, bool),
    Const(CapstoneInt)
}

#[derive(Clone)]
enum CapstoneInsn {
    Li(CapstoneReg, CapstoneImm),
    Sd(CapstoneReg, CapstoneReg, CapstoneImm),
    Ld(CapstoneReg, CapstoneReg, CapstoneImm),
    Stc(CapstoneReg, CapstoneReg, CapstoneImm),
    Ldc(CapstoneReg, CapstoneReg, CapstoneImm),
    Scc(CapstoneReg, CapstoneReg),
    Scco(CapstoneReg, CapstoneReg),
    Lcc(CapstoneReg, CapstoneReg),
    Lcco(CapstoneReg, CapstoneReg),
    Lcb(CapstoneReg, CapstoneReg),
    Lce(CapstoneReg, CapstoneReg),
    Lcn(CapstoneReg, CapstoneReg),
    Add(CapstoneReg, CapstoneReg),
    Sub(CapstoneReg, CapstoneReg),
    Mult(CapstoneReg, CapstoneReg),
    Div(CapstoneReg, CapstoneReg),
    Jz(CapstoneReg, CapstoneReg),
    Eq(CapstoneReg, CapstoneReg, CapstoneReg),
    Le(CapstoneReg, CapstoneReg, CapstoneReg),
    Lt(CapstoneReg, CapstoneReg, CapstoneReg),
    And(CapstoneReg, CapstoneReg),
    Splitlo(CapstoneReg, CapstoneReg, CapstoneReg),
    Mov(CapstoneReg, CapstoneReg),
    Or(CapstoneReg, CapstoneReg),
    Mrev(CapstoneReg, CapstoneReg),
    Call(CapstoneReg, CapstoneReg),
    Revoke(CapstoneReg),
    Delin(CapstoneReg),
    Tighten(CapstoneReg, CapstoneReg),
    Shrink(CapstoneReg, CapstoneReg, CapstoneReg),
    Seal(CapstoneReg),
    SealRet(CapstoneReg, CapstoneReg),
    Drop(CapstoneReg),
    Ret(CapstoneReg, CapstoneReg),
    RetSealed(CapstoneReg, CapstoneReg),
    Jmp(CapstoneReg),
    Out(CapstoneReg),
    Halt
}

impl CapstoneReg {
    fn try_from_str(s: &str) -> Option<CapstoneReg> {
        if s.starts_with('r') && !s.starts_with("re") {
            Some(CapstoneReg::Gpr(u8::from_str(&s[1..]).ok()?))
        } else{
            match s {
                "pc" => Some(CapstoneReg::Pc),
                "ret" => Some(CapstoneReg::Ret),
                "epc" => Some(CapstoneReg::Epc),
                _ => None
            }
        }
    }
}


#[derive(Debug)]
struct CapstoneFunctionType {
    pinned_gprs: Vec<CapstoneReg>
}

impl Default for CapstoneFunctionType {
    fn default() -> CapstoneFunctionType {
        CapstoneFunctionType { pinned_gprs: Vec::new() }
    }
}


impl CapstoneFunctionType {
    fn from_specifiers(specifiers: &Vec<Node<DeclarationSpecifier>>) -> CapstoneFunctionType {
        let mut func_type = CapstoneFunctionType::default();
        for spec in specifiers.iter() {
            match &spec.node {
                DeclarationSpecifier::Extension(extensions) => {
                    for ext in extensions.iter() {
                        match &ext.node {
                            Extension::Attribute(attr) => {
                                match attr.name.node.as_str() {
                                    "pinned" => {
                                        func_type.pinned_gprs.extend(attr.arguments.iter().
                                                filter_map(|x| x.node.capstone_try_into_str().and_then(|s| CapstoneReg::try_from_str(s.trim_matches('\"')))));
                                    }
                                    _ => {}
                                }
                            }
                            _ => { }
                        }
                    }
                }
                _ => { }
            }
        }
        func_type
    }
}

impl Display for CapstoneImm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            CapstoneImm::NTag(tag, false) => {
                tag.fmt(f)
            }
            CapstoneImm::STag(tag, false) => {
                tag.fmt(f)
            }
            CapstoneImm::NTag(tag, true) => {
                write!(f, "{}", tag)
            }
            CapstoneImm::STag(tag, true) => {
                write!(f, "{}", tag)
            }
            CapstoneImm::Const(c) => {
                c.fmt(f)
            }
        }
    }
}

#[derive(Clone, Debug)]
struct CapstoneRegResult {
    reg: CapstoneReg,
    release_strategy: CapstoneRegRelease,
    data_type: CapstoneType
}

impl CapstoneRegResult {
    fn new_simple(reg: CapstoneReg, dtype: CapstoneType) -> CapstoneRegResult {
        CapstoneRegResult { reg, release_strategy: CapstoneRegRelease::Simple, data_type: dtype}
    }

    fn set_writeback(&mut self, wb_to: CapstoneRegResult, offset: CapstoneOffset) {
        self.release_strategy = CapstoneRegRelease::WriteBack(Box::new(wb_to), offset);
    }

    fn set_type(&mut self, data_type: CapstoneType) {
        self.data_type = data_type;
    }

    fn to_simple(&self) -> CapstoneRegResult {
        CapstoneRegResult { reg: self.reg, release_strategy: CapstoneRegRelease::Simple, data_type: self.data_type.clone() }
    }
}

#[derive(Clone, Debug)]
enum CapstoneRegRelease {
    Simple,
    WriteBack(Box<CapstoneRegResult>, CapstoneOffset) // the reliant capability register and the offset
}

impl Display for CapstoneInsn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            CapstoneInsn::Li(r, v) => {
                match v {
                    CapstoneImm::NTag(n, _) => {
                        write!(f, "la {}, {}", r, n.0)
                    }
                    CapstoneImm::STag(n, _) => {
                        write!(f, "lla {}, {}", r, n.0)
                    }
                    CapstoneImm::Const(c) => {
                        write!(f, "li {}, {}", r, c)
                    }
                }
            }
            CapstoneInsn::Sd(rd, rs, offset) => {
                write!(f, "sd {}, {}({})", rs, offset, rd)
            }
            CapstoneInsn::Ld(rd, rs, offset) => {
                write!(f, "ld {}, {}({})", rd, offset, rs)
            }
            CapstoneInsn::Stc(rs2, rs1, offset) => {
                write!(f, ".insn s 0x5b, 0x4, {}, {}({})  # stc {}, {}({})", rs2, offset, rs1, rs2, offset, rs1)
            }
            CapstoneInsn::Ldc(rd, rs, offset) => {
                write!(f, ".insn i 0x5b, 0x3, {}, {}({})  # ldc {}, {}({})", rd, offset, rs, rd, offset, rs)
            }
            CapstoneInsn::Scc(rd, rs) => {
                write!(f, ".insn r 0x5b, 0x1, 0x5, {}, {}, {}  # scc {}, {}, {}", rd, rd, rs, rd, rd, rs)
            }
            CapstoneInsn::Scco(rd, rs) => {
                // write!(f, "scco {}, {}", rd, rs)
                write!(f, "# scco")
            }
            CapstoneInsn::Lcc(rd, rs) => {
                // write!(f, "lcc {}, {}", rd, rs)
                write!(f, "# lcc")
            }
            CapstoneInsn::Lcco(rd, rs) => {
                // write!(f, "lcco {}, {}", rd, rs)
                write!(f, "# lcco")
            }
            CapstoneInsn::Add(rd, rs) => {
                write!(f, "add {}, {}, {}", rd, rd, rs)
            }
            CapstoneInsn::Sub(rd, rs) => {
                write!(f, "sub {}, {}, {}", rd, rd, rs)
            }
            CapstoneInsn::Mult(rd, rs) => {
                write!(f, "mul {}, {}, {}", rd, rd, rs)
            }
            CapstoneInsn::Div(rd, rs) => {
                write!(f, "div {}, {}, {}", rd, rd, rs)
            }
            CapstoneInsn::Jz(rd, rs) => {
                write!(f, "beq {}, {}, zero", rd, rs)
            }
            CapstoneInsn::Jmp(r) => {
                write!(f, "jalr zero, {}, 0", r)
            }
            CapstoneInsn::Out(r) => {
                write!(f, ".insn r 0x5b, 0x1, 0x43, zero, {}, zero  # print({})", r, r)
            }
            CapstoneInsn::Halt => {
                // write!(f, "halt")
                write!(f, "# halt")
            }
            CapstoneInsn::Le(rd, r1, r2) => {
                // write!(f, "le {} {} {}", rd, r1, r2)
                write!(f, "# le")
            }
            CapstoneInsn::Lt(rd, r1, r2) => {
                // write!(f, "lt {} {} {}", rd, r1, r2)
                write!(f, "# lt")
            }
            CapstoneInsn::Eq(rd, r1, r2) => {
                // write!(f, "eq {} {} {}", rd, r1, r2)
                write!(f, "# eq")
            }
            CapstoneInsn::And(rd, rs) => {
                write!(f, "and {}, {}, {}", rd, rd, rs)
            }
            CapstoneInsn::Or(rd, rs) => {
                write!(f, "or {}, {}, {}", rd, rd, rs)
            }
            CapstoneInsn::Splitlo(rd, rs, rp) => {
                // write!(f, "splitlo {} {} {}", rd, rs, rp)
                write!(f, "# splitlo")
            }
            CapstoneInsn::Lcb(rd, rs) => {
                // write!(f, "lcb {} {}", rd, rs)
                write!(f, "# lcb")
            }
            CapstoneInsn::Lce(rd, rs) => {
                // write!(f, "lce {} {}", rd, rs)
                write!(f, "# lce")
            }
            CapstoneInsn::Lcn(rd, rs) => {
                // write!(f, "lcn {} {}", rd, rs)
                write!(f, "# lcn")
            }
            CapstoneInsn::Mrev(rd, rs) => {
                // write!(f, "mrev {} {}", rd, rs)
                write!(f, "# mrev")
            }
            CapstoneInsn::Mov(rd, rs) => {
                write!(f, "mv {}, {}", rd, rs)
            }
            CapstoneInsn::Seal(r) => {
                write!(f, ".insn r 0x5b, 0x1, 0x7, {}, {}, zero  # seal {}, {}", r, r, r, r)
            }
            CapstoneInsn::SealRet(rd, rs) => {
                // write!(f, "sealret {} {}", rd, rs)
                write!(f, "# sealret")
            }
            CapstoneInsn::Call(r, ra) => {
                write!(f, ".insn r 0x5b, 0x1, 0x20, {}, {}, zero  # call {}, {}", ra, r, ra, r)
            }
            CapstoneInsn::Revoke(r) => {
                // write!(f, "revoke {}", r)
                write!(f, "# revoke")
            }
            CapstoneInsn::Delin(r) => {
                // write!(f, "delin {}", r)
                write!(f, "# delin")
            }
            CapstoneInsn::Tighten(rd, rs) => {
                // write!(f, "tighten {} {}", rd, rs)
                write!(f, "# tighten")
            }
            CapstoneInsn::Shrink(rd, rb, re) => {
                // write!(f, "shrink {} {} {}", rd, rb, re)
                write!(f, "# shrink")
            }
            CapstoneInsn::Drop(r) => {
                // write!(f, "drop {}", r)
                write!(f, "# drop")
            }
            CapstoneInsn::Ret(rd, rs) => {
                write!(f, ".insn r 0x5b, 0x1, 0x21, {}, {}, zero  # return {}, {}", rd, rs, rd, rs)
            }
            CapstoneInsn::RetSealed(rd, rs) => {
                write!(f, ".insn r 0x5b, 0x1, 0x21, {}, {}, zero  # return {}, {}", rd, rs, rd, rs)
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct CapstoneNTag(u32);

#[derive(Clone, Debug)]
struct CapstoneSTag(String);

enum CapstoneAssemblyUnit {
    Tag(CapstoneNTag),
    Insn(CapstoneInsn),
    Passthrough(String),
    Comment(String)
}

impl Display for CapstoneSTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}:", self.0)
    }
}

impl Display for CapstoneNTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:", self.0)
    }
}

impl CapstoneAssemblyUnit {
    fn print_code(&self, mem_offset: &mut u32) {
        match &self {
            CapstoneAssemblyUnit::Tag(tag) => {
                println!("{}", tag);
            }
            CapstoneAssemblyUnit::Insn(insn) => {
                println!("  {}", insn);
                *mem_offset += 1;
            }
            CapstoneAssemblyUnit::Passthrough(s) => {
                println!("  {}", s);
                *mem_offset += 1;
            }
            CapstoneAssemblyUnit::Comment(s) => {
                if CLI_ARGS.print_comments {
                    println!("# {}", s);
                }
            }
        }
    }
}


struct CapstoneFunction {
    name: String, // name of the function
    code: Vec<CapstoneAssemblyUnit>,
    return_type: CapstoneType
}

impl CapstoneFunction {
    fn new(name: &str, _func_type: CapstoneFunctionType, return_type: CapstoneType) -> CapstoneFunction {
        CapstoneFunction {
            name: name.to_string(),
            code: Vec::new(),
            return_type
        }
    }

    fn push_asm_unit(&mut self, asm_unit: CapstoneAssemblyUnit) {
        self.code.push(asm_unit);
    }

    fn print_code(&self, mem_offset: &mut u32) {
        println!("{}:", self.name);
        for asm_unit in self.code.iter() {
            asm_unit.print_code(mem_offset);
        }
    }
}

#[derive(Debug, Clone)]
struct CapstoneVariable {
    offset_in_cap: CapstoneOffset, // offset inside the capability
    ttype: CapstoneType,
    cap: CapstoneRegResult, // actuall register that has the capability for loading the variable
}

#[derive(Debug, Clone)]
struct CapstoneUnresolvedVar(String); // this is the unresolved variable. We don't know where to find it yet. We know nothing more than the name
    // we are assuming that the member operators are left associative

impl CapstoneVariable {
    fn join_indirect(&self, other: &CapstoneUnresolvedVar, ctx: &mut CodeGenContext) -> Option<CapstoneEvalResult> {
        match &self.ttype {
            CapstoneType::Cap(base_type) => { // indirect join can only be performed on a capability
                match base_type {
                    Some(box CapstoneType::Struct(base_struct_name)) => {
                        let reg_lhs = ctx.gen_ld_alloc(self); // holds a capability
                        let (offset, ttype) = ctx.resolve_var(CapstoneOffset::Const(0), &Some(base_struct_name.to_string()), other)?;
                        Some(CapstoneEvalResult::Variable(CapstoneVariable {
                            offset_in_cap: offset,
                            ttype,
                            cap: reg_lhs.clone()
                        }))
                    } 
                    _ => None
                }
            }
            _ => None
        }
    }
    fn join_direct(&self, other: &CapstoneUnresolvedVar, ctx: &mut CodeGenContext) -> Option<CapstoneEvalResult> {
        match &self.ttype {
            CapstoneType::Cap(_) => {
                match other.0.as_str() {
                    "cursor" => {
                        ctx.gen_cap_query(self, CapstoneInsn::Lcc)
                    }
                    "offset" => {
                        ctx.gen_cap_query(self, CapstoneInsn::Lcco)
                    }
                    "base" => {
                        ctx.gen_cap_query(self, CapstoneInsn::Lcb)
                    }
                    "end" => {
                        ctx.gen_cap_query(self, CapstoneInsn::Lce)
                    }
                    "size" => {
                        ctx.gen_cap_query(self, CapstoneInsn::Lcn)
                    }
                    _ => None
                }
            }
            CapstoneType::Struct(parent_name) => {
                let (offset, ttype) = ctx.resolve_var(self.offset_in_cap, &Some(parent_name.to_string()), other).expect("Field not found!");
                Some(CapstoneEvalResult::Variable(CapstoneVariable { offset_in_cap: offset, 
                    ttype,
                    cap: self.cap.clone()}))
            }
            _ => {
                None
            }
        }
    }
}

type CapstoneScope = HashMap<String, (u32, CapstoneField)>;

#[derive(Debug, Clone)]
enum CapstoneEvalResult {
    Const(CapstoneInt),
    Register(CapstoneRegResult),
    UnresolvedVar(CapstoneUnresolvedVar),
    Variable(CapstoneVariable) // parent offset, parent type, name
}

impl CapstoneEvalResult {
    fn get_type(&self) -> &CapstoneType {
        match self {
            CapstoneEvalResult::Const(_) => &CapstoneType::Int,
            CapstoneEvalResult::Register(reg) => &reg.data_type,
            CapstoneEvalResult::Variable(var) => &var.ttype,
            CapstoneEvalResult::UnresolvedVar(_var) => &CapstoneType::Int, // resolve first, otherwise fall back to int
        }
    }

    fn try_into_variable(&self, ctx: &mut CodeGenContext) -> Option<CapstoneVariable> {
        match self {
            CapstoneEvalResult::Variable(var) => Some(var.clone()),
            CapstoneEvalResult::UnresolvedVar(var) => {
                ctx.resolve_top_var(var)
            },
            _ => None
        }
    }


    fn resolve(&mut self, ctx: &mut CodeGenContext) {
        if let CapstoneEvalResult::UnresolvedVar(var) = self {
            *self = CapstoneEvalResult::Variable(ctx.resolve_top_var(&var).expect("Failed to resolve variable!"));
        }
    }

    fn to_register(&self, ctx: &mut CodeGenContext) -> CapstoneRegResult {
        self.to_register_with_offset(0, ctx).unwrap() // should not fail at offset 0
    }

    fn to_register_with_offset(&self, offset: u32, ctx: &mut CodeGenContext) -> Option<CapstoneRegResult> {
        if self.get_size(ctx) <= offset {
            return None;
        }
        match self {
            &CapstoneEvalResult::Const(n) => {
                Some(ctx.gen_li_alloc(CapstoneImm::Const(n)))
            }
            CapstoneEvalResult::Register(r) => {
                Some(r.clone())
            }
            CapstoneEvalResult::Variable(var) => {
                // load the variable
                Some(ctx.gen_ld_alloc_extra_offset(var, &CapstoneOffset::Const(offset))) // note: here the type and size 
                    // information will be lost
            }
            CapstoneEvalResult::UnresolvedVar(unresolved_var) => {
                let r = ctx.resolve_top_var(unresolved_var).map(|x| ctx.gen_ld_alloc_extra_offset(&x, &CapstoneOffset::Const(offset)));
                if r.is_some() {
                    r
                } else{
                    Some(ctx.gen_li_alloc(CapstoneImm::STag(CapstoneSTag(unresolved_var.0.clone()), false)))
                }
            }
        }
    }
    fn join_indirect(&self, other: &CapstoneEvalResult, ctx: &mut CodeGenContext) -> Option<CapstoneEvalResult> {
        if let CapstoneEvalResult::UnresolvedVar(rhs) = other { // the rhs can only be an unresolved variable
            match self {
                CapstoneEvalResult::Variable(var) => {
                    var.join_indirect(rhs, ctx)
                }
                CapstoneEvalResult::UnresolvedVar(var) => {
                    ctx.resolve_top_var(var)?.join_indirect(rhs, ctx)
                }
                _ => {
                    None
                }
            }
        } else{
            None
        }
    }
    fn join_direct(&self, other: &CapstoneEvalResult, ctx: &mut CodeGenContext) -> Option<CapstoneEvalResult> {
        if let CapstoneEvalResult::UnresolvedVar(rhs) = other {
            match self {
                CapstoneEvalResult::Variable(var) => {
                    var.join_direct(rhs, ctx)
                }
                CapstoneEvalResult::UnresolvedVar(var) => {
                    ctx.resolve_top_var(var)?.join_direct(rhs, ctx)
                }
                _ => {
                    None
                }
            }
        } else {
            None
        }
    }
    fn get_size(&self, ctx: &mut CodeGenContext) -> u32 {
        match self {
            CapstoneEvalResult::Const(_) => 1,
            CapstoneEvalResult::Register(_) => 1,
            CapstoneEvalResult::Variable(var) => {
                var.ttype.get_size(&ctx.struct_map)
            }
            CapstoneEvalResult::UnresolvedVar(var) => {
                ctx.resolve_top_var(var).map(|x| x.ttype.get_size(&ctx.struct_map)).unwrap_or(1)
            }
        }
    }
}

trait CapstoneEvaluator {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult;
}

trait CapstoneEmitter {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext);
}

trait CapstoneLiteralConstruct<T> {
    fn to_capstone_literal(&self) -> T;
}

impl CapstoneLiteralConstruct<CapstoneInt> for Integer {
    fn to_capstone_literal(&self) -> CapstoneInt {
        // ignoring suffix and base for now
        CapstoneInt::from_str(&self.number).unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
enum CapstoneOffset {
    Const(u32),
    Register(CapstoneReg)
}

impl CapstoneOffset {
    fn to_register(&self, ctx: &mut CodeGenContext) -> CapstoneRegResult {
        match self {
            CapstoneOffset::Const(c) => ctx.gen_li_alloc(CapstoneImm::Const(*c as u64)),
            CapstoneOffset::Register(r) => CapstoneRegResult::new_simple(*r, CapstoneType::Int)
        }
    }
    fn add(&self, other: &CapstoneOffset, ctx: &mut CodeGenContext) -> CapstoneOffset {
        match (self, other) {
            // if both are constants known at compile time, we just add the constants together
            (CapstoneOffset::Const(c1), CapstoneOffset::Const(c2)) => CapstoneOffset::Const(c1 + c2),
            _ => {
                // otherwise, we have to compute at runtime
                let r1 = self.to_register(ctx);
                let r2 = other.to_register(ctx);
                ctx.push_insn(CapstoneInsn::Add(r1.reg, r2.reg));
                ctx.release_gpr(&r2);
                CapstoneOffset::Register(r1.reg)
            }
        }
    }
    fn drop(self, ctx: &mut CodeGenContext) {
        if let CapstoneOffset::Register(reg) = self {
            ctx.release_gpr(&CapstoneRegResult::new_simple(reg, CapstoneType::Int));
        }
    }

    fn from_reg_result(res: &CapstoneRegResult, _ctx: &mut CodeGenContext) -> CapstoneOffset {
        CapstoneOffset::Register(res.reg)
    }

    fn duplicate(&self, ctx: &mut CodeGenContext) -> CapstoneOffset {
        match self {
            CapstoneOffset::Const(n) => CapstoneOffset::Const(*n),
            CapstoneOffset::Register(reg) =>
                CapstoneOffset::Register(ctx.gen_mov_alloc(&CapstoneRegResult::new_simple(*reg, CapstoneType::Int)).reg)
        }
    }
}

struct CodeGenContext {
    tag_count: u32,
    init_func: CapstoneFunction,
    cur_func: Option<CapstoneFunction>,
    functions: Vec<CapstoneFunction>,
    variables: Vec<CapstoneScope>,
    reserved_stack_size: u32,
    gprs: [CapstoneRegState; CAPSTONE_GPR_N],
    break_conts: Vec<(CapstoneNTag, CapstoneNTag)>, // break and continue stack
    in_func: bool,
    struct_map: CapstoneStructMap,
}

#[derive(Copy, Clone, Debug)]
enum CapstoneOpRAB {
    Le,
    Lt,
    Ge,
    Gt,
    Eq
}

#[derive(Copy, Clone, Debug)]
enum CapstoneOpAB {
    Plus,
    Minus,
    And,
    Mult,
    Div,
    Or
}


impl CodeGenContext {
    fn new() -> CodeGenContext {
        let mut instance = CodeGenContext {
            tag_count : 0,
            functions: Vec::new(),
            cur_func: None,
            variables: vec![CapstoneScope::new()],
            reserved_stack_size: 0,
            gprs: [CapstoneRegState::Free; CAPSTONE_GPR_N],
            in_func: false,
            init_func: CapstoneFunction::new("_init", CapstoneFunctionType::default(), CapstoneType::Void),
            break_conts: Vec::new(),
            struct_map: CapstoneStructMap::new()
        };
        if let CapstoneReg::Gpr(n) = CAPSTONE_STACK_REG {
            instance.gprs[n as usize] = CapstoneRegState::Pinned; // reserve the gpr for the stack cap
        }
        if let CapstoneReg::Gpr(n) = CAPSTONE_RA_REG {
            instance.gprs[n as usize] = CapstoneRegState::Pinned; // reserve the gpr for the return address cap
        }
        instance
    }

    fn release_gprs(&mut self, gprs: &Vec<CapstoneRegResult>) {
        for gpr in gprs.iter() {
            self.release_gpr(&gpr);
        }
    }

    fn gen_index(&mut self, lhs: &mut CapstoneEvalResult, rhs: &mut CapstoneEvalResult) -> CapstoneEvalResult {
        rhs.resolve(self);
        lhs.resolve(self);
        match (lhs.get_type(), rhs.get_type()) {
            (CapstoneType::Cap(base_type), CapstoneType::Int) => {
                let rhs_reg = rhs.to_register(self); // it probably makes more sense to generate rhs first
                let lhs_reg = lhs.to_register(self);
                let base_size = base_type.as_ref().map_or(1, |t| t.get_size(&self.struct_map));
                let rsize = self.gen_li_alloc(CapstoneImm::Const(base_size as u64));
                self.push_insn(CapstoneInsn::Mult(rsize.reg, rhs_reg.reg));
                self.release_gpr(&rhs_reg);
                CapstoneEvalResult::Variable(CapstoneVariable { 
                    offset_in_cap: CapstoneOffset::from_reg_result(&rsize, self),
                    ttype: base_type.as_ref().map_or(CapstoneType::Int, |x| (**x).clone()),
                    cap: lhs_reg
                })
            }
            (CapstoneType::Array(base_type, _len), CapstoneType::Int) => {
                let lhs_var = lhs.try_into_variable(self).expect("Bad index op!");
                let rhs_reg = rhs.to_register(self);
                let base_size = base_type.get_size(&self.struct_map);
                let rsize = self.gen_li_alloc(CapstoneImm::Const(base_size as u64));
                self.push_insn(CapstoneInsn::Mult(rsize.reg, rhs_reg.reg));
                self.release_gpr(&rhs_reg);
                CapstoneEvalResult::Variable(CapstoneVariable {
                    offset_in_cap: lhs_var.offset_in_cap.add(&CapstoneOffset::Register(rsize.reg), self),
                    ttype: *base_type.clone(),
                    cap: lhs_var.cap.clone()
                })
            }
            _ => {
                CapstoneEvalResult::Const(0)
            }
        }
    }


    fn add_var_to_scope(&mut self, field: CapstoneField) {
        let offset = self.reserved_stack_size;
        self.reserved_stack_size += field.get_size(&self.struct_map);
        self.variables.first_mut().unwrap().insert(field.name.clone(), (offset, field));
    }

    fn find_var_in_scope(&self, var_name: &str) -> Option<(u32, CapstoneType)> {
        self.variables.iter().map(|x| x.get(var_name)).fold(None, |x, y| x.or(y.map(|v| (v.0, v.1.field_type.clone()))))
    }

    fn pop_scope(&mut self) {
        self.variables.pop();
    }

    fn push_scope(&mut self) {
        self.variables.push(CapstoneScope::new());
    }

    fn push_asm_unit(&mut self, asm_unit: CapstoneAssemblyUnit) {
        if self.in_func {
            self.cur_func.as_mut().expect("Function list is empty!")
        } else {
            &mut self.init_func
        }.push_asm_unit(asm_unit);
    }

    fn enter_function(&mut self, func_name: &str, func_type: CapstoneFunctionType, func_return_type: CapstoneType) {
        self.in_func = true;
        self.reserved_stack_size = 0;
        self.reset_gprs();
        for pinned_gpr in func_type.pinned_gprs.iter() {
            self.pin_gpr(*pinned_gpr);
        }
        self.cur_func = Some(CapstoneFunction::new(func_name, func_type, func_return_type));
    }

    fn exit_function(&mut self) {
        self.gen_return(&CapstoneEvalResult::Const(0), &CapstoneEvalResult::Const(0));
        self.reset_gprs();

        let cur_func = std::mem::take(&mut self.cur_func).expect("Error exit_function: current function not found!");
        self.functions.push(cur_func);
        self.in_func = false;
    }


    fn grab_new_gpr(&mut self) -> Option<CapstoneRegResult> {
        let some_n = self.gprs.iter().enumerate().find(|&(_idx, &b)| b.is_available()).map(|x| x.0 as u8);
        if let &Some(n) = &some_n {
            self.gprs[n as usize].grab();
        }
        some_n.map(|x| CapstoneRegResult::new_simple(CapstoneReg::Gpr(x), CapstoneType::Int))
    }

    fn get_gpr_state(&self, reg: CapstoneReg) -> CapstoneRegState {
        match reg {
            CapstoneReg::Gpr(n) => self.gprs[n as usize],
            _ => CapstoneRegState::Pinned
        }
    }

    fn release_gpr(&mut self, reg: &CapstoneRegResult) {
        match &reg.release_strategy {
            CapstoneRegRelease::Simple => {
                // no need to do anything
                self.release_gpr_no_recurse(reg);
            }
            CapstoneRegRelease::WriteBack(wb_to, offset) => {
                self.gen_store_with_cap(offset, wb_to, &CapstoneEvalResult::Register(reg.to_simple()));
                self.release_gpr_no_recurse(reg);
                offset.drop(self);
                self.release_gpr(wb_to); // TODO: change to loops
            }
        }
    }

    fn release_gpr_ancestors(&mut self, reg: &CapstoneRegResult) {
        match &reg.release_strategy {
            CapstoneRegRelease::WriteBack(wb_to, _) => {
                self.release_gpr(wb_to);
            }
            _ => { }
        }
    }

    fn release_gpr_no_recurse(&mut self, reg: &CapstoneRegResult) {
        if reg.reg != CAPSTONE_STACK_REG {
            if let &CapstoneReg::Gpr(n) = &reg.reg {
                self.gprs[n as usize].free();
            }
        }
    }

    fn pin_gpr(&mut self, reg: CapstoneReg) {
        if let CapstoneReg::Gpr(n) = reg {
            self.gprs[n as usize].pin();
        }
    }

    fn reset_gprs(&mut self) {
        self.gprs.iter_mut().for_each(|x| x.reset());
        self.pin_gpr(CAPSTONE_STACK_REG);
    }

    fn gen_li(&mut self, reg: &CapstoneRegResult, val: CapstoneImm) {
        self.push_insn(CapstoneInsn::Li(reg.reg,
            val));
    }

    fn gen_li_alloc(&mut self, val: CapstoneImm) -> CapstoneRegResult {
        let reg = self.grab_new_gpr().expect("Gpr allocation for li failed!");
        self.gen_li(&reg, val);
        reg
    }

    fn gen_ld_extra_offset(&mut self, reg: &mut CapstoneRegResult, var: &CapstoneVariable, offset: &CapstoneOffset) {
        if var.ttype.is_cap() {
            let dup_offset = var.offset_in_cap.duplicate(self);
            let cap_reg = self.gen_load_cap_extra_offset(var, offset).expect("Failed to obtain capability for ld!");
            self.push_insn(CapstoneInsn::Ld(reg.reg, cap_reg.reg, CapstoneImm::Const(0)));
            reg.set_writeback(cap_reg, dup_offset.add(&offset, self));
        } else{
            let cap_reg = self.gen_load_cap_extra_offset(var, offset).expect("Failed to obtain capability for ld!");
            self.push_insn(CapstoneInsn::Ld(reg.reg, cap_reg.reg, CapstoneImm::Const(0)));
            self.release_gpr(&cap_reg); // TODO: we should not reuse cap registers like this
        }
        reg.set_type(var.ttype.clone());
        // it is necessary to write them back if the capability is linear
    }

    fn gen_ld_alloc_extra_offset(&mut self, var: &CapstoneVariable, offset: &CapstoneOffset) -> CapstoneRegResult {
        let mut reg = self.grab_new_gpr().expect("Gpr allocation for ld failed!");
        self.gen_ld_extra_offset(&mut reg, var, offset);
        reg
    }

    fn gen_ld_alloc(&mut self, var: &CapstoneVariable) -> CapstoneRegResult {
        self.gen_ld_alloc_extra_offset(var, &CapstoneOffset::Const(0))
    }

    // the var is stored on stack
    fn resolve_top_var(&mut self, var: &CapstoneUnresolvedVar) -> Option<CapstoneVariable> {
        let (offset, ttype) = self.resolve_var(CapstoneOffset::Const(0), &None, var)?;
        Some(CapstoneVariable {
            offset_in_cap: offset,
            ttype,
            cap: CapstoneRegResult::new_simple(CAPSTONE_STACK_REG, CapstoneType::Cap(None))
        })
    }

    fn resolve_var(&mut self, offset: CapstoneOffset, parent_struct: &Option<String>, var: &CapstoneUnresolvedVar) -> Option<(CapstoneOffset, CapstoneType)> {
        match parent_struct {
            Some(parent_name) => {
                let par = self.struct_map.get(parent_name)?;
                par.get_field(&var.0).map(|(o, t)| (offset.add(&CapstoneOffset::Const(o), self), t))
            }
            None => {
                self.find_var_in_scope(&var.0).map(|(o, t)| (offset.add(&CapstoneOffset::Const(o), self), t))
            }
        }
    }

    fn gen_load_cap_extra_offset(&mut self, var: &CapstoneVariable, extra_offset: &CapstoneOffset) -> Option<CapstoneRegResult> {
        let offset_reg = var.offset_in_cap.add(extra_offset, self).to_register(self);
        let cap_reg = var.cap.reg; // TODO: change this to a CapstoneRegResult to keep track of writeback
        self.push_insn(CapstoneInsn::Scco(cap_reg, offset_reg.reg));
        self.release_gpr(&offset_reg);
        // TODO: handle writeback here
        Some(var.cap.clone())
    }

    fn gen_store_with_cap(&mut self, offset: &CapstoneOffset, rcap: &CapstoneRegResult, val: &CapstoneEvalResult) {
        let rvalue_size = val.get_size(self);
        for i in 0..rvalue_size {
            let reg = val.to_register_with_offset(i, self).unwrap();
            self.release_gpr_ancestors(&reg); // since there is no need to write the value back, we release the ancestors in advance
            let noffset = &offset.add(&CapstoneOffset::Const(i), self);
            self.gen_sd_imm_offset(rcap, &reg, &noffset);
            self.release_gpr_no_recurse(&reg);
        }
    }

    fn gen_store(&mut self, var: &CapstoneVariable, val: &CapstoneEvalResult) -> CapstoneEvalResult {
        let rvalue_size = val.get_size(self);
        for i in 0..rvalue_size {
            let reg = val.to_register_with_offset(i, self).unwrap();
            let cap_reg = self.gen_load_cap_extra_offset(var, &CapstoneOffset::Const(i)).expect("Unable to access variable through capabilities!");
            self.push_insn(CapstoneInsn::Sd(cap_reg.reg, reg.reg, CapstoneImm::Const(0)));
            self.release_gpr(&reg);
            self.release_gpr(&cap_reg);
        }
        val.clone()
    }

    fn gen_store_drop_result(&mut self, var: &CapstoneVariable, val: &CapstoneEvalResult) {
        let res = self.gen_store(var, val);
        self.drop_result(&res);
    }

    fn gen_assignment(&mut self, lhs: &CapstoneEvalResult, rhs: &CapstoneEvalResult) -> CapstoneEvalResult {
        match &lhs {
            CapstoneEvalResult::Variable(var) => {
                self.gen_store(var, rhs)
            }
            CapstoneEvalResult::UnresolvedVar(var) => {
                let var = self.resolve_top_var(var).expect("Failed to resolve variable for assignment!");
                self.gen_store(&var, rhs)
            }
            CapstoneEvalResult::Register(reg) => {
                // this can happen in reg()
                let rhs_reg = rhs.to_register(self);
                self.push_insn(CapstoneInsn::Mov(reg.reg, rhs_reg.reg)); // then we simply move the value and we are done
                self.release_gpr(&rhs_reg);
                lhs.clone()
            }
            _ => {
                eprintln!("Unsupported lvalue for assignment: {:?}", lhs);
                CapstoneEvalResult::Const(0)
            }
        }
    }

    fn gen_a_b(&mut self, op: CapstoneOpAB, lhs: &CapstoneEvalResult, rhs: &CapstoneEvalResult) -> CapstoneEvalResult {
        let rd = lhs.to_register(self);
        let rs = rhs.to_register(self);
        self.push_insn(match op {
            CapstoneOpAB::Plus => {
                CapstoneInsn::Add(rd.reg, rs.reg)
            }
            CapstoneOpAB::Minus => {
                CapstoneInsn::Sub(rd.reg, rs.reg)
            }
            CapstoneOpAB::And => {
                CapstoneInsn::And(rd.reg, rs.reg)
            }
            CapstoneOpAB::Mult => {
                CapstoneInsn::Mult(rd.reg, rs.reg)
            }
            CapstoneOpAB::Div => {
                CapstoneInsn::Div(rd.reg, rs.reg)
            }
            CapstoneOpAB::Or => {
                CapstoneInsn::Or(rd.reg, rs.reg)
            }
        });
        self.release_gpr(&rs);
        CapstoneEvalResult::Register(rd)
    }

    fn print_code(&self) {
        let mut mem_offset = 0;
        println!(".section .text");
        println!(".global _init");
        println!(".global _start");
        self.init_func.print_code(&mut mem_offset);
        println!();
        for func in self.functions.iter() {
            func.print_code(&mut mem_offset);
            println!();
        }
        // stack
        println!(".section .data");
        println!("_stack:");
        println!("  .zero 4096 * 4");
    }

    fn drop_result(&mut self, res: &CapstoneEvalResult) {
        if let CapstoneEvalResult::Register(reg) = res {
            self.release_gpr(&reg);
        }
    }

    fn alloc_tag(&mut self) -> CapstoneNTag {
        let tag = CapstoneNTag(self.tag_count);
        self.tag_count += 1;
        tag
    }
    
    fn push_break_cont(&mut self, break_tag: CapstoneNTag, cont_tag: CapstoneNTag) {
        self.break_conts.push((break_tag, cont_tag));
    }

    fn pop_break_cont(&mut self) {
        self.break_conts.pop();
    }
    
    fn gen_jmp_tag(&mut self, tag: CapstoneNTag) {
        let target_reg = self.gen_li_alloc(CapstoneImm::NTag(tag, true));
        self.push_insn(CapstoneInsn::Jmp(target_reg.reg));
        self.release_gpr(&target_reg);
    }

    fn gen_break(&mut self) {
        if let Some(break_tag) = self.break_conts.first().map(|x| x.0) {
            self.gen_jmp_tag(break_tag);
        } else{
            eprintln!("No break tag found!");
        }
    }

    fn gen_continue(&mut self) {
        if let Some(cont_tag) = self.break_conts.first().map(|x| x.0) {
            self.gen_jmp_tag(cont_tag);
        } else{
            eprintln!("No continue tag found!");
        }
    }

    fn add_struct(&mut self, capstone_struct: CapstoneStruct) {
        self.struct_map.insert(capstone_struct.name.clone(), capstone_struct);
    }


    fn gen_r_a_b(&mut self, op: CapstoneOpRAB, lhs: &CapstoneEvalResult, rhs: &CapstoneEvalResult) -> CapstoneEvalResult {
        let lhs_reg = lhs.to_register(self);
        let rhs_reg = rhs.to_register(self);
        let res_reg = self.grab_new_gpr().expect("Failed to allocate gpr for eq!");
        self.push_insn(match op {
            CapstoneOpRAB::Le => {
                CapstoneInsn::Le(res_reg.reg, lhs_reg.reg, rhs_reg.reg)
            }
            CapstoneOpRAB::Lt => {
                CapstoneInsn::Lt(res_reg.reg, lhs_reg.reg, rhs_reg.reg)
            }
            CapstoneOpRAB::Ge => {
                CapstoneInsn::Le(res_reg.reg, rhs_reg.reg, lhs_reg.reg)
            }
            CapstoneOpRAB::Gt => {
                CapstoneInsn::Lt(res_reg.reg, rhs_reg.reg, lhs_reg.reg)
            }
            CapstoneOpRAB::Eq => {
                CapstoneInsn::Eq(res_reg.reg, lhs_reg.reg, rhs_reg.reg)
            }
        });
        self.release_gpr(&lhs_reg);
        self.release_gpr(&rhs_reg);
        CapstoneEvalResult::Register(res_reg)
    }

    fn gen_not(&mut self, r: &CapstoneEvalResult) -> CapstoneEvalResult {
        self.gen_r_a_b(CapstoneOpRAB::Eq, r, &CapstoneEvalResult::Const(0))
    }

    fn push_comment(&mut self, comment: &str) {
        self.push_asm_unit(CapstoneAssemblyUnit::Comment(comment.to_string()));
    }

    fn push_insn(&mut self, insn: CapstoneInsn) {
        self.push_asm_unit(CapstoneAssemblyUnit::Insn(insn));
    }

    fn push_tag(&mut self, tag: CapstoneNTag) {
        self.push_asm_unit(CapstoneAssemblyUnit::Tag(tag));
    }
    
    fn push_passthrough_asm(&mut self, s: String) {
        self.push_asm_unit(CapstoneAssemblyUnit::Passthrough(s));
    }

    fn gen_init_func_pre(&mut self) {
        // self.push_asm_unit(CapstoneAssemblyUnit::Passthrough("delin pc".to_string()));
        let stack_reg = CapstoneRegResult::new_simple(CAPSTONE_STACK_REG, CapstoneType::Cap(None));
        let rheap = self.gen_splitlo_alloc_const(&stack_reg, CLI_ARGS.stack_size);

        // setting the heap offset to 0
        let r = self.gen_li_alloc(CapstoneImm::Const(0));
        self.push_insn(CapstoneInsn::Scco(rheap.reg, r.reg));

        self.gen_store_with_cap(&CapstoneOffset::Const(0), &stack_reg, &CapstoneEvalResult::Register(rheap));
        self.release_gpr(&r);
    }

    fn gen_init_func_post(&mut self) {
        // jump to the main function
        let r = self.gen_li_alloc(CapstoneImm::STag(CapstoneSTag("_start".to_string()), true));
        self.push_insn(CapstoneInsn::Jmp(r.reg));
        self.release_gpr(&r);
    }

    fn gen_mrev_alloc(&mut self, r: &CapstoneRegResult) -> CapstoneRegResult {
        let rrev = self.grab_new_gpr().expect("Failed to allocate GPR for mrev!");
        self.push_insn(CapstoneInsn::Mrev(rrev.reg, r.reg));
        rrev
    }


    fn gen_splitlo_alloc(&mut self, rs: &CapstoneRegResult, roffset: &CapstoneRegResult) -> CapstoneRegResult {
        let rsplit = self.grab_new_gpr().expect("Failed to allocate GPR for splitl");
        self.push_insn(CapstoneInsn::Splitlo(rsplit.reg, rs.reg, roffset.reg));
        rsplit
    }

    fn gen_splitlo_alloc_const(&mut self, rs: &CapstoneRegResult, offset: u32) -> CapstoneRegResult {
        let roffset = self.gen_li_alloc(CapstoneImm::Const(offset as u64));
        let res = self.gen_splitlo_alloc(rs, &roffset);
        self.release_gpr(&roffset);
        res
    }

    fn gen_splitlo_alloc_const_reversible(&mut self, rs: &CapstoneRegResult, offset: u32) -> (CapstoneRegResult, CapstoneRegResult) {
        let rrev = self.gen_mrev_alloc(rs);
        (self.gen_splitlo_alloc_const(rs, offset), rrev)
    }

    fn gen_sd_imm_offset(&mut self, rcap: &CapstoneRegResult, rs: &CapstoneRegResult, offset: &CapstoneOffset) {
        let roffset = offset.to_register(self);
        self.push_insn(CapstoneInsn::Scco(rcap.reg, roffset.reg));
        self.release_gpr(&roffset);
        self.push_insn(CapstoneInsn::Sd(rcap.reg, rs.reg, CapstoneImm::Const(0)));
    }

    fn gen_seal_setup(&mut self, rseal: &CapstoneRegResult, rpc: &CapstoneRegResult, _rstack: &CapstoneRegResult) {
        self.gen_sd_imm_offset(rseal, rpc, &CapstoneOffset::Const(CAPSTONE_SEALED_OFFSET_PC as u32));
        self.release_gpr(&rpc);
        // We instead pass the stack as the argument
        self.push_insn(CapstoneInsn::Seal(rseal.reg));
    }

    fn gen_mov_alloc(&mut self, rs: &CapstoneRegResult) -> CapstoneRegResult {
        let rd = self.grab_new_gpr().expect("Failed to allocate GPR for mov!");
        self.push_insn(CapstoneInsn::Mov(rd.reg, rs.reg));
        rd
    }

    fn gen_load_with_cap_alloc(&mut self, offset: &CapstoneOffset, rcap: &CapstoneRegResult) -> CapstoneRegResult {
        let r = offset.to_register(self);
        self.push_insn(CapstoneInsn::Scco(rcap.reg, r.reg));
        self.push_insn(CapstoneInsn::Ld(r.reg, rcap.reg, CapstoneImm::Const(0)));
        r
    }

    fn gen_call_stack_setup(&mut self, rstack: &CapstoneRegResult, args: &Vec<Node<Expression>>) {
        let mut offset = 0;
        for arg in args.iter() {
            let res = arg.capstone_evaluate(self);
            self.gen_store_with_cap(&CapstoneOffset::Const(offset), rstack, &res);
            offset += res.get_size(self);
        }
    }

    // do not release sealed
    fn gen_call_on_reg(&mut self, sealed: &CapstoneRegResult, args: &Vec<Node<Expression>>) -> CapstoneEvalResult {
        let (rstack_callee, rrev) = self.gen_splitlo_alloc_const_reversible(
            &CapstoneRegResult::new_simple(CAPSTONE_STACK_REG, CapstoneType::Cap(None)), self.reserved_stack_size);
        let rstack_rev = self.gen_mrev_alloc(&rstack_callee);
        self.gen_call_stack_setup(&rstack_callee, args);
        self.push_insn(CapstoneInsn::Call(sealed.reg, rstack_callee.reg));
        self.release_gpr(&rstack_callee);

        // restore the stack
        self.push_insn(CapstoneInsn::Revoke(rstack_rev.reg));
        let res = self.gen_load_with_cap_alloc(&CapstoneOffset::Const(0), &rstack_rev);
        self.push_insn(CapstoneInsn::Drop(rstack_rev.reg)); // TODO: if the result is an uninitialised capability, initialise it first
        self.release_gpr(&rstack_rev);

        self.push_insn(CapstoneInsn::Drop(CAPSTONE_STACK_REG));
        self.push_insn(CapstoneInsn::Revoke(rrev.reg));
        self.push_insn(CapstoneInsn::Mov(CAPSTONE_STACK_REG, rrev.reg));
        self.release_gpr(&rrev);

        CapstoneEvalResult::Register(res)
    }

    fn gen_call_in_group(&mut self, func_name: &str, args: &Vec<Node<Expression>>) -> CapstoneEvalResult {
        let (rseal, rrev) = self.gen_splitlo_alloc_const_reversible(
            &CapstoneRegResult::new_simple(CAPSTONE_STACK_REG, CapstoneType::Cap(None)), self.reserved_stack_size);
        let (rstack_callee, rrev2) = self.gen_splitlo_alloc_const_reversible(&rseal, CAPSTONE_SEALED_REGION_SIZE as u32);

        let rstack_rev = self.gen_mrev_alloc(&rstack_callee);
        // now rstack_callee contains a linear capability that points to a region of size
        // CAPSTONE_SEALED_REGION_SIZE
        // let rpc = self.gen_mov_alloc(&CapstoneRegResult::new_simple(CapstoneReg::Pc, CapstoneType::Cap(None)));
        let rpc = self.gen_mov_alloc(&CapstoneRegResult::new_simple(CapstoneReg::Gpr(0), CapstoneType::Cap(None)));
        let pc_addr_imm = CapstoneImm::STag(CapstoneSTag(func_name.to_string()), false);
        let rpc_addr = self.gen_li_alloc(pc_addr_imm);
        self.push_insn(CapstoneInsn::Scc(rpc.reg, rpc_addr.reg));
        self.release_gpr(&rpc_addr);

        // set up the sealed capability
        self.gen_call_stack_setup(&rstack_callee, args);
        self.gen_seal_setup(&rseal, &rpc, &rstack_callee);
        self.push_insn(CapstoneInsn::Call(rseal.reg, rstack_callee.reg));
        self.release_gpr(&rstack_callee);
        self.release_gpr(&rseal);

        // restore the stack/
        self.push_insn(CapstoneInsn::Revoke(rstack_rev.reg));
        let res = self.gen_load_with_cap_alloc(&CapstoneOffset::Const(0), &rstack_rev);
        self.push_insn(CapstoneInsn::Drop(rstack_rev.reg)); // TODO: if the result is an uninitialised capability, initialise it first
        self.release_gpr(&rstack_rev);

        self.push_insn(CapstoneInsn::Revoke(rrev2.reg));
        self.push_insn(CapstoneInsn::Drop(rrev2.reg));
        self.release_gpr(&rrev2);

        self.push_insn(CapstoneInsn::Drop(CAPSTONE_STACK_REG));
        self.push_insn(CapstoneInsn::Revoke(rrev.reg));
        self.push_insn(CapstoneInsn::Mov(CAPSTONE_STACK_REG, rrev.reg));
        self.release_gpr(&rrev);

        CapstoneEvalResult::Register(res)
    }

    fn in_cur_func_group(&self, _func_name: &str) -> bool {
        true
    }

    fn gen_retval_setup(&mut self, retval: &CapstoneEvalResult) {
        match &self.cur_func.as_ref().unwrap().return_type {
            CapstoneType::Void => {
                // We don't really need to do anything here
            }
            _ => {
                self.gen_store_with_cap(&CapstoneOffset::Const(0), 
                    &CapstoneRegResult::new_simple(CAPSTONE_STACK_REG, CapstoneType::Cap(None)), retval);
                self.push_insn(CapstoneInsn::Drop(CAPSTONE_STACK_REG));
            }
        }
    }

    // ordinary return
    // return value should be passed through the stack
    fn gen_return(&mut self, retval: &CapstoneEvalResult, sealed_repl: &CapstoneEvalResult) {
        self.gen_retval_setup(retval);
        let r = sealed_repl.to_register(self);
        // Temporary solution for now: allowing dropping of uninitialised caps
        self.push_insn(CapstoneInsn::Drop(CAPSTONE_STACK_REG));
        self.release_gpr_ancestors(&r);
        self.push_insn(CapstoneInsn::Ret(CAPSTONE_RA_REG, r.reg));
        self.release_gpr_no_recurse(&r);
    }

    fn gen_return_sealed(&mut self, retval: &CapstoneEvalResult, new_pc: &CapstoneEvalResult) {
        self.gen_retval_setup(retval);
        let r = new_pc.to_register(self);
        self.push_insn(CapstoneInsn::Drop(CAPSTONE_STACK_REG));
        self.release_gpr_ancestors(&r);
        self.push_insn(CapstoneInsn::RetSealed(CAPSTONE_RA_REG, r.reg));
        self.release_gpr(&r);
    }

    fn gen_cap_query(&mut self, cap_var: &CapstoneVariable,
                     insn_gen: fn(CapstoneReg, CapstoneReg) -> CapstoneInsn) -> Option<CapstoneEvalResult> {
        let rcap = self.gen_ld_alloc(cap_var);
        let res = self.grab_new_gpr().expect("Failed to allocate GPR for cap query!");
        self.push_insn(insn_gen(res.reg, rcap.reg));
        // we need to put it back in case it is a linear capability
        self.release_gpr(&rcap);
        Some(CapstoneEvalResult::Register(res))
    }
}


impl<T: CapstoneEmitter> CapstoneEmitter for Node<T> {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        self.node.capstone_emit_code(ctx);
    }
}

impl CapstoneEvaluator for Constant {
   fn capstone_evaluate(&self, _ctx: &mut CodeGenContext) -> CapstoneEvalResult {
       match &self {
           Constant::Integer(int) => {
               CapstoneEvalResult::Const(int.to_capstone_literal())
           }
           _ => {
               CapstoneEvalResult::Const(0)
           }
       }
   }
}

impl CapstoneEvaluator for BinaryOperatorExpression {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        let mut lhs = self.lhs.capstone_evaluate(ctx);
        let mut rhs = self.rhs.capstone_evaluate(ctx);
        match self.operator.node {
            BinaryOperator::Assign => {
                ctx.gen_assignment(&lhs, &rhs)
            }
            BinaryOperator::Plus => {
                ctx.gen_a_b(CapstoneOpAB::Plus, &lhs, &rhs)
            }
            BinaryOperator::Minus => {
                ctx.gen_a_b(CapstoneOpAB::Minus, &lhs, &rhs)
            }
            BinaryOperator::Multiply => {
                ctx.gen_a_b(CapstoneOpAB::Mult, &lhs, &rhs)
            }
            BinaryOperator::Divide => {
                ctx.gen_a_b(CapstoneOpAB::Div, &lhs, &rhs)
            }
            BinaryOperator::LogicalAnd => {
                ctx.gen_a_b(CapstoneOpAB::And, &lhs, &rhs)
            }
            BinaryOperator::LogicalOr => {
                ctx.gen_a_b(CapstoneOpAB::Or, &lhs, &rhs)
            }
            BinaryOperator::NotEquals => {
                let res = ctx.gen_r_a_b(CapstoneOpRAB::Eq, &lhs, &rhs);
                ctx.gen_not(&res)
            }
            BinaryOperator::Equals => {
                ctx.gen_r_a_b(CapstoneOpRAB::Eq, &lhs, &rhs)
            }
            BinaryOperator::LessOrEqual => {
                ctx.gen_r_a_b(CapstoneOpRAB::Le, &lhs, &rhs)
            }
            BinaryOperator::Greater => {
                ctx.gen_r_a_b(CapstoneOpRAB::Gt, &lhs, &rhs)
            }
            BinaryOperator::GreaterOrEqual => {
                ctx.gen_r_a_b(CapstoneOpRAB::Ge, &lhs, &rhs)
            }
            BinaryOperator::Less => {
                ctx.gen_r_a_b(CapstoneOpRAB::Lt, &lhs, &rhs)
            }
            BinaryOperator::Index => {
                ctx.gen_index(&mut lhs, &mut rhs)
            }
            _ => {
                CapstoneEvalResult::Const(0)
            }
        }
    }
}

impl CapstoneEvaluator for Identifier {
    fn capstone_evaluate(&self, _ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        CapstoneEvalResult::UnresolvedVar(CapstoneUnresolvedVar(self.name.to_string()))
    }
}

trait ToRegisters {
    fn to_registers(&self, ctx: &mut CodeGenContext) -> Vec<CapstoneRegResult>;
}

impl ToRegisters for Vec<Node<Expression>> {
    fn to_registers(&self, ctx: &mut CodeGenContext) -> Vec<CapstoneRegResult> {
        self.iter().map(|x| x.capstone_evaluate(ctx).to_register(ctx)).collect()
    }
}

trait CapstoneTryIntoStr {
    fn capstone_try_into_str(&self) -> Option<&str>;
}

impl CapstoneTryIntoStr for Expression {
    fn capstone_try_into_str(&self) -> Option<&str> {
        match self {
            Expression::StringLiteral(s) => {
                s.node.first().map(|x| x.as_str())
            }
            _ => None
        }
    }
}


/**
 * calling conventions: arguments are copied in order at the bottom of the called
 * stack frame. Upon return, any arguments that are references should be dropped.
 *
 * capability manipulation: each capability always has a underlying type, which is the type
 * of object the memory region contains. We also need to provide some builtin functions.
 *
 * Each function should be inside a separate code capability. This would require us to 
 * decide how capabilities are initialised first.
 *
 *
 * More generally, each function belongs to a function group. A function group corresponds to
 * a single code capability. The assignment of function groups shall be known at compile
 * time so the compiler can generate code differently when a function call occurs within the same
 * function group vs outside the function group.
 * */
impl CapstoneEvaluator for CallExpression {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        ctx.push_comment("Call expression");
        ctx.push_comment("Evaluating callee");
        let callee = self.callee.capstone_evaluate(ctx);
        ctx.push_comment("Evaluating arguments");
        ctx.push_comment("Arguments evaluated");
        let res = match &callee {
            CapstoneEvalResult::UnresolvedVar(unresolved_var) => {
                match unresolved_var.0.as_str() {
                    // builtin pseudo calls
                    "print" => {
                        let args = self.arguments.to_registers(ctx);
                        let r = args.first()
                            .expect("Missing argument for print!");
                        ctx.push_insn(CapstoneInsn::Out(r.reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "exit" => {
                        ctx.push_insn(CapstoneInsn::Halt);
                        CapstoneEvalResult::Const(0)
                    }
                    "splitlo" => {
                        let args = self.arguments.to_registers(ctx);
                        let res = CapstoneEvalResult::Register(ctx.gen_splitlo_alloc(&args[0], &args[1]));
                        ctx.release_gprs(&args);
                        res
                    }
                    "seal" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Seal(args.first().expect("Missing argument for seal!").reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "sealret" => {
                        let arg_rd = self.arguments.first().expect("Missing arguments for sealret!").capstone_evaluate(ctx).to_register(ctx);
                        let reg_name: &str = &*self.arguments[1].node.capstone_try_into_str().expect("Wrong argument type for reg!");
                        let arg_rs = CapstoneReg::try_from_str(reg_name.trim_matches('\"')).expect("Unknown register name!");
                        ctx.push_insn(CapstoneInsn::SealRet(arg_rd.reg, arg_rs));
                        ctx.release_gpr(&arg_rd);
                        CapstoneEvalResult::Const(0)
                    }
                    "revoke" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Revoke(args.first().expect("Missing argument for revoke!").reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "delin" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Delin(args.first().expect("Missing argument for delin!").reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "tighten" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Tighten(args[0].reg, args[1].reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "shrink" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Shrink(args[0].reg, args[1].reg, args[2].reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "scco" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Scco(args[0].reg, args[1].reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "scc" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Scc(args[0].reg, args[1].reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "mrev" => {
                        let args = self.arguments.to_registers(ctx);
                        let reg = ctx.gen_mrev_alloc(args.first().expect("Missing argument for mrev!"));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Register(reg)
                    }
                    "drop" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(CapstoneInsn::Drop(args.first().expect("Missing argument for drop!").reg));
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    "reg" => {
                        let s: &str = &*self.arguments.first().expect("Missing argument for reg!").node.capstone_try_into_str().expect("Wrong argument type for reg!");
                        let reg = CapstoneReg::try_from_str(s.trim_matches('\"')).expect("Unknown register name!");
                        if !matches!(ctx.get_gpr_state(reg), CapstoneRegState::Pinned) {
                            eprintln!("Warning: register {} is not pinned", reg);
                        }
                        CapstoneEvalResult::Register(CapstoneRegResult::new_simple(reg, CapstoneType::Int))
                    }
                    "returnsl" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.gen_return_sealed(&CapstoneEvalResult::Register(args[0].clone()), 
                            &CapstoneEvalResult::Register(args[1].clone()));
                        CapstoneEvalResult::Const(0)
                    }
                    "direct_call" => {
                        // does not do anything beyond a call instruction
                        let args = self.arguments.to_registers(ctx);
                        let dummy_arg = ctx.gen_li_alloc(CapstoneImm::Const(0));
                        ctx.push_insn(CapstoneInsn::Call(args[0].reg, dummy_arg.reg));
                        ctx.release_gpr(&dummy_arg);
                        ctx.release_gprs(&args);
                        CapstoneEvalResult::Const(0)
                    }
                    _ => {
                        match ctx.resolve_top_var(&unresolved_var) {
                            Some(var) => {
                                let reg = ctx.gen_ld_alloc(&var);
                                let res = ctx.gen_call_on_reg(&reg, &self.arguments);
                                ctx.release_gpr(&reg);
                                res
                            }
                            None => {
                                if ctx.in_cur_func_group(&unresolved_var.0){
                                    ctx.gen_call_in_group(&unresolved_var.0, &self.arguments)
                                } else{
                                    eprintln!("Cross-group function call not supported yet: {}", &unresolved_var.0);
                                    CapstoneEvalResult::Const(0)
                                }
                            }
                        }
                    }
                }
            }
            CapstoneEvalResult::Variable(var) => {
                let reg = ctx.gen_ld_alloc(var);
                let res = ctx.gen_call_on_reg(&reg, &self.arguments);
                ctx.release_gpr(&reg);
                res
            }
            CapstoneEvalResult::Register(reg) => {
                let res = ctx.gen_call_on_reg(&reg, &self.arguments);
                ctx.release_gpr(&reg);
                res
            }
            _ => {
                eprintln!("Function pointer not supported: {:?}", self.callee);
                CapstoneEvalResult::Const(0)
            }
        };
        ctx.drop_result(&callee);
        res
    }
}

impl CapstoneEvaluator for UnaryOperatorExpression {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        let exp = self.operand.capstone_evaluate(ctx);
        match self.operator.node {
            UnaryOperator::Negate => {
                ctx.gen_not(&exp)
            }
            UnaryOperator::Indirection => {
                // TODO: *cap expression
                CapstoneEvalResult::Const(0)
            }
            UnaryOperator::Address => {
                // TODO: &obj expresion
                CapstoneEvalResult::Const(0) // support stack obj and in function call arguments only
            }
            _ => {
                exp
            }
        }
    }
}

impl CapstoneEvaluator for MemberExpression {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        let lhs = self.expression.capstone_evaluate(ctx);
        let rhs = self.identifier.capstone_evaluate(ctx);
        match self.operator.node {
            MemberOperator::Direct => {
                // a.b
                // for this type, directly compute the offset
                lhs.join_direct(&rhs, ctx).expect("Bad direct member expression!")
            }
            MemberOperator::Indirect => {
                lhs.join_indirect(&rhs, ctx).expect("Bad indirect member expression!")
            }
        }
    }
}

impl CapstoneEvaluator for Expression {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        match &self {
            Expression::Constant(con) => {
                con.capstone_evaluate(ctx)
            }
            Expression::UnaryOperator(op) => {
                op.capstone_evaluate(ctx)
            }
            Expression::BinaryOperator(op) => {
                // TODO: we can enable some binary operations on either 
                // capabilities or the fields of capabilities
                // Example: + can be used to move cursor
                op.capstone_evaluate(ctx)
            }
            Expression::Identifier(id) => {
                id.capstone_evaluate(ctx)
            }
            Expression::Statement(stmt) => {
                stmt.capstone_emit_code(ctx);
                CapstoneEvalResult::Const(0)
            }
            Expression::Call(call) => {
                call.capstone_evaluate(ctx)
            }
            Expression::Comma(exps) => {
                let mut last_res = CapstoneEvalResult::Const(0);
                for e in exps.iter() {
                    ctx.drop_result(&last_res);
                    last_res = e.capstone_evaluate(ctx);
                }
                last_res
            }
            Expression::Member(member) => {
                member.capstone_evaluate(ctx)
            }
            Expression::SizeOfTy(st) => {
                let ttype = CapstoneType::parse(ctx, &st.node.0.node).expect("Bad type specifier in sizeof!");
                CapstoneEvalResult::Const(ttype.get_size(&ctx.struct_map) as u64)
            }
            _ => {
                CapstoneEvalResult::Const(0)
            }
        }
    }
}

impl<T: CapstoneEvaluator> CapstoneEvaluator for Node<T> {
    fn capstone_evaluate(&self, ctx: &mut CodeGenContext) -> CapstoneEvalResult {
        self.node.capstone_evaluate(ctx)
    }
}



impl CapstoneEmitter for InitDeclarator {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        let name = get_decl_kind_name(&self.declarator.node.kind.node).expect("Bad variable name!");

        let init_val = match &self.initializer {
            Some(n) => {
                match &n.node {
                    Initializer::Expression(exp) => {
                        exp.capstone_evaluate(ctx)
                    }
                    _ => {
                        eprintln!("Literal format unsupported: {:?}", self.initializer);
                        CapstoneEvalResult::Const(0)
                    }
                }
            }
            None => {
                CapstoneEvalResult::Const(0)
            }
        };
        let var = ctx.resolve_top_var(&CapstoneUnresolvedVar(name.to_string()))
            .expect("Failed to resolve var name for initialization!");
        ctx.gen_store_drop_result(&var, &init_val);
    }
}

#[derive(Debug)]
struct CapstoneStruct {
    size: u32,
    name: String,
    members: CapstoneScope,
}


type CapstoneStructMap = HashMap<String, CapstoneStruct>;

impl CapstoneStruct {
    fn new(name: String, _struct_map: &CapstoneStructMap) -> CapstoneStruct {
        let res = CapstoneStruct {
            size: 0,
            name: name.clone(),
            members: CapstoneScope::new()
        };
        res
    }

    fn add_field(&mut self, field: CapstoneField, struct_map: &CapstoneStructMap) {
        let field_size = field.get_size(struct_map);
        self.members.insert(field.name.clone(), (self.size, field));
        self.size += field_size;
    }

    fn get_field(&self, name: &str) -> Option<(u32, CapstoneType)> {
        self.members.get(name).map(|(o, f)| (*o, f.field_type.clone()))
    }
}

// TODO: add a special type: capability
#[derive(Clone, Debug)]
enum CapstoneType {
    Int,
    Void,
    Cap(Option<Box<CapstoneType>>), // might be another capability associated with another type
    Struct(String),
    Array(Box<CapstoneType>, u32)
}

impl CapstoneType {
    fn get_size(&self, struct_map: &CapstoneStructMap) -> u32 {
        match self {
            CapstoneType::Int => 1,
            CapstoneType::Void => 1,
            CapstoneType::Cap(_) => 1,
            CapstoneType::Struct(struct_name) => {
                struct_map.get(struct_name).expect("Struct undefined!").size
            },
            CapstoneType::Array(base_type, size) => {
                base_type.get_size(struct_map) * size
            }
        }
    }

    fn is_cap(&self) -> bool {
        match self {
            CapstoneType::Cap(_) => true,
            _ => false
        }
    }

    fn derived_decorate(&mut self, ctx: &mut CodeGenContext, derived: &DerivedDeclarator) {
        match &derived {
            DerivedDeclarator::Pointer(_) => {
                *self = CapstoneType::Cap(Some(Box::new(self.clone())));
            }
            DerivedDeclarator::Array(array_decl) => {
                let size = match &array_decl.node.size {
                    ArraySize::VariableExpression(e) => {
                        let rsize = e.capstone_evaluate(ctx);
                        match rsize {
                            CapstoneEvalResult::Const(size) => size,
                            _ => 1
                        }
                    }
                    _ => 1,
                };
                *self = CapstoneType::Array(Box::new(self.clone()), size as u32);
            }
            _ => {
            }
        }
    }

    fn parse<T:AsDeclaration>(ctx: &mut CodeGenContext, declaration: &T) -> Option<CapstoneType> {
        let mut t = declaration.get_specifiers().first()?.to_capstone_type(ctx);
        for decl in declaration.get_declarators().iter() {
            for d in decl.derived.iter() {
                t.derived_decorate(ctx, &d.node);
            }
        }
        Some(t)
    }
}


trait ToCapstoneType {
    fn to_capstone_type(&self, ctx: &mut CodeGenContext) -> CapstoneType;
}

#[derive(Debug)]
struct CapstoneField {
    name: String,
    field_type: CapstoneType,
}


trait AsDeclaration {
    fn get_declarators(&self) -> Vec<&Declarator>;
    fn get_specifiers(&self) -> Vec<&TypeSpecifier>;
}

impl AsDeclaration for FunctionDefinition {
    fn get_declarators(&self) -> Vec<&Declarator> {
        vec![&self.declarator.node]
    }
    
    fn get_specifiers(&self) -> Vec<&TypeSpecifier> {
        self.specifiers.iter().filter_map(|x| {
            match &x.node {
                DeclarationSpecifier::TypeSpecifier(t) => {
                    Some(&t.node)
                }
                _ => None
            }
        }).collect()
    }
}

impl AsDeclaration for TypeName {
    fn get_declarators(&self) -> Vec<&Declarator> {
        match self.declarator.as_ref() {
            None => Vec::new(),
            Some(d) => {
                vec![&d.node]
            }
        }
    }

    fn get_specifiers(&self) -> Vec<&TypeSpecifier> {
        self.specifiers.iter().filter_map(|x| {
            match &x.node {
                SpecifierQualifier::TypeSpecifier(type_spec) => {
                    Some(&type_spec.node)
                }
                _ => {
                    None
                }
            }
        }).collect()
    }
}

impl AsDeclaration for Declaration {
    fn get_declarators(&self) -> Vec<&Declarator> {
        self.declarators.iter().map(|x| &x.node.declarator.node).collect()
    }

    fn get_specifiers(&self) -> Vec<&TypeSpecifier> {
        self.specifiers.iter().filter_map(|x| {
            match &x.node {
                DeclarationSpecifier::TypeSpecifier(type_spec) => {
                    Some(&type_spec.node)
                }
                _ => {
                    None
                }
            }
        }).collect()
    }
}

impl AsDeclaration for ParameterDeclaration {
    fn get_declarators(&self) -> Vec<&Declarator> {
        self.declarator.as_ref().iter().map(|x| &x.node).collect()
    }

    fn get_specifiers(&self) -> Vec<&TypeSpecifier> {
        self.specifiers.iter().filter_map(|x| {
            match &x.node {
                DeclarationSpecifier::TypeSpecifier(type_spec) => {
                    Some(&type_spec.node)
                }
                _ => {
                    None
                }
            }
        }).collect()
    }
}

impl AsDeclaration for StructField {
    fn get_declarators(&self) -> Vec<&Declarator> {
        self.declarators.iter().filter_map(|x| x.node.declarator.as_ref().map(|d| &d.node)).collect()
    }
    fn get_specifiers(&self) -> Vec<&TypeSpecifier> {
        self.specifiers.iter().filter_map(|x| {
            match &x.node {
                SpecifierQualifier::TypeSpecifier(type_spec) => {
                    Some(&type_spec.node)
                }
                _ => {
                    None
                }
            }
        }).collect()
    }
}

impl CapstoneField {
    fn parse_from_declarator(ctx: &mut CodeGenContext, decl: &Declarator, base_type: CapstoneType) 
        -> CapstoneField{
        let name = match &decl.kind.node {
            DeclaratorKind::Identifier(id) => {
                &id.node.name
            }
            _ => {
                panic!("No name specified for a field!");
            }
        };
        let mut ttype = base_type;
        for derived in decl.derived.iter() {
            ttype.derived_decorate(ctx, &derived.node);
        }
        CapstoneField { name: name.to_string(), field_type: ttype }
    }

    fn parse<T:AsDeclaration> (ctx: &mut CodeGenContext, struct_field: &T) -> Vec<CapstoneField>  {
        // only accept single specifier and declarator for now
        let field_type = struct_field.get_specifiers().first()
            .expect("No specifier for struct fields!").to_capstone_type(ctx);
        struct_field.get_declarators().iter().map(|x| {
            CapstoneField::parse_from_declarator(ctx, &x, field_type.clone())
        }).collect()
    }
}

impl CapstoneField {
    fn get_size(&self, struct_map: &CapstoneStructMap) -> u32 {
        self.field_type.get_size(struct_map)
    }
}


impl ToCapstoneType for StructType {
    fn to_capstone_type(&self, ctx: &mut CodeGenContext) -> CapstoneType {
        if self.kind.node != StructKind::Struct {
            eprintln!("Union is not supported!");
        }
        let id = self.identifier.as_ref().expect("Anonymous struct not supported!");

        if let Some(decl) = &self.declarations {
            // if this comes with declarations
            let mut capstone_struct = CapstoneStruct::new(id.node.name.clone(), &mut ctx.struct_map);
            for f in decl.iter() {
                match &f.node {
                    StructDeclaration::Field(field) => {
                        let fields = CapstoneField::parse(ctx, &field.node);
                        for field in fields.into_iter() {
                            capstone_struct.add_field(field, &ctx.struct_map);
                        }
                    }
                    _ => {
                        eprintln!("Static assertion in struct not supported!");
                    }
                }
            }
            ctx.add_struct(capstone_struct);
        }

        CapstoneType::Struct(id.node.name.clone())
    }
}

impl ToCapstoneType for TypeSpecifier {
    fn to_capstone_type(&self, ctx: &mut CodeGenContext) -> CapstoneType {
        match self {
            TypeSpecifier::Struct(struct_node) => {
                struct_node.to_capstone_type(ctx)
            }
            TypeSpecifier::Void => {
                CapstoneType::Void
            }
            _ => {
                CapstoneType::Int
            }
        }
    }
}

impl<T: ToCapstoneType> ToCapstoneType for Node<T> {
    fn to_capstone_type(&self, ctx: &mut CodeGenContext) -> CapstoneType {
        self.node.to_capstone_type(ctx)
    }
}

impl CapstoneEmitter for Declaration {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        let fields = CapstoneField::parse(ctx, self);
        for field in fields.into_iter() {
            ctx.add_var_to_scope(field);
        }
        for n in self.declarators.iter() {
            n.capstone_emit_code(ctx);
        }
    }
}

impl CapstoneEmitter for BlockItem {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        match &self {
            BlockItem::Declaration(n) => {
                n.capstone_emit_code(ctx);
            }
            BlockItem::Statement(n) => {
                n.capstone_emit_code(ctx);
            }
            _ => {
                eprintln!("{:?} not processed", self);
            }
        }
    }
}

impl CapstoneEmitter for IfStatement {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        let tag_else = ctx.alloc_tag();
        let reg_cond = self.condition.capstone_evaluate(ctx).to_register(ctx);

        let reg_else = ctx.gen_li_alloc(CapstoneImm::NTag(tag_else, true));
        ctx.push_insn(CapstoneInsn::Jz(reg_else.reg, reg_cond.reg));
        ctx.release_gpr(&reg_cond);
        ctx.release_gpr(&reg_else);
        
        // then clause
        ctx.push_scope();
        self.then_statement.capstone_emit_code(ctx);
        ctx.pop_scope();

        ctx.push_tag(tag_else);

        if let Some(else_stmt) = &self.else_statement {
            // continuation of then clause: skip the else clause
            let tag_end = ctx.alloc_tag();
            let reg_end = ctx.gen_li_alloc(CapstoneImm::NTag(tag_end, true));
            ctx.push_insn(CapstoneInsn::Jmp(reg_end.reg));
            ctx.release_gpr(&reg_end);

            // else clause
            ctx.push_tag(tag_else);

            ctx.push_scope();
            else_stmt.capstone_emit_code(ctx);
            ctx.pop_scope();
            
            // end
            ctx.push_tag(tag_end);
        }
    }
}

impl CapstoneEmitter for WhileStatement {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        let tag_start = ctx.alloc_tag();
        let tag_end = ctx.alloc_tag();

        ctx.push_tag(tag_start);
        let reg_cond = self.expression.capstone_evaluate(ctx).to_register(ctx);
        let reg_end = ctx.gen_li_alloc(CapstoneImm::NTag(tag_end, true));
        ctx.push_insn(CapstoneInsn::Jz(reg_end.reg, reg_cond.reg));
        ctx.release_gpr(&reg_end);
        ctx.release_gpr(&reg_cond);

        // while body
        ctx.push_scope();
        ctx.push_break_cont(tag_end, tag_start);
        self.statement.capstone_emit_code(ctx);
        ctx.pop_break_cont();
        ctx.pop_scope();
        
        // end
        ctx.gen_jmp_tag(tag_start);
        ctx.push_tag(tag_end);
    }
}

// TODO: add do-while statement

impl CapstoneEmitter for AsmStatement {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        match self {
            AsmStatement::GnuBasic(basic_asm) => {
                for asm in basic_asm.node.iter() {
                    ctx.push_passthrough_asm(asm.trim_matches('\"').to_string());
                }
            }
            _ => {
                eprintln!("Extended inline asm not supported: {:?}", self);
            }
        }
    }
}

impl CapstoneEmitter for Statement {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        match &self {
            Statement::Compound(v) => {
                for n in v.iter() {
                    n.capstone_emit_code(ctx);
                }
            }
            Statement::Expression(exp) => {
                if let Some(e) = exp {
                    let res = e.capstone_evaluate(ctx);
                    ctx.drop_result(&res);
                }
            }
            Statement::If(if_stmt) => {
                if_stmt.capstone_emit_code(ctx);
            }
            Statement::While(while_stmt) => {
                while_stmt.capstone_emit_code(ctx);
            }
            Statement::Break => {
                ctx.gen_break();
            }
            Statement::Continue => {
                ctx.gen_continue();
            }
            Statement::Asm(asm_stmt) => {
                asm_stmt.capstone_emit_code(ctx); 
            }
            Statement::Return(ret) => {
                let retval =
                    if let Some(exp) = ret {
                        exp.capstone_evaluate(ctx)
                    } else{
                        CapstoneEvalResult::Const(0)
                    };
                ctx.gen_return(&retval, &CapstoneEvalResult::Const(0));
            }
            _ => {
                eprintln!("Unsupported statement: {:?}", self);
            }
        }
    }
}

fn get_decl_kind_name(kind: &DeclaratorKind) -> Option<&str> {
    match kind {
        DeclaratorKind::Identifier(n) => {
            Some(&n.node.name)
        }
        _ => {
            None
        }
    }
}

impl CapstoneEmitter for ParameterDeclaration {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        let fields = CapstoneField::parse(ctx, self);
        for field in fields.into_iter() {
            ctx.add_var_to_scope(field);
        }
    }
}

impl CapstoneEmitter for FunctionDeclarator {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        for param in self.parameters.iter() {
            param.capstone_emit_code(ctx);
        }
    }
}

impl CapstoneEmitter for DerivedDeclarator {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        match self {
            DerivedDeclarator::Function(func_declarator) => {
                func_declarator.capstone_emit_code(ctx);
            }
            _ => {}
        }
    }
}

impl CapstoneEmitter for Declarator {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        for derived in self.derived.iter() {
            derived.capstone_emit_code(ctx);
        }
    }
}

impl CapstoneEmitter for FunctionDefinition {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        let func_name = get_decl_kind_name(&self.declarator.node.kind.node)
            .expect("Bad function name!");
        let func_type = CapstoneFunctionType::from_specifiers(&self.specifiers);
        let func_return_type = CapstoneType::parse(ctx, self).expect("Function return type missing!");
        // emit code for body
        ctx.enter_function(func_name, func_type, func_return_type);
        ctx.push_scope();
        self.declarator.capstone_emit_code(ctx);
        self.statement.capstone_emit_code(ctx);
        ctx.pop_scope();
        ctx.exit_function();
    }
}

impl CapstoneEmitter for ExternalDeclaration {
    fn capstone_emit_code(&self, ctx: &mut CodeGenContext) {
        match self {
            ExternalDeclaration::Declaration(n) => {
                n.capstone_emit_code(ctx);
            }
            ExternalDeclaration::StaticAssert(_n) => {
            }
            ExternalDeclaration::FunctionDefinition(n) => {
                n.capstone_emit_code(ctx);
            }
        }
    }
}

fn generate_code(trans_unit: &TranslationUnit) {
    let mut ctx = CodeGenContext::new();
    ctx.gen_init_func_pre();
    for node in trans_unit.0.iter() {
        node.capstone_emit_code(&mut ctx);
    }
    ctx.gen_init_func_post();
    // output the code
    ctx.print_code();
}


fn main() {
    let config = Config::default();
    match parse(&config, &CLI_ARGS.source) {
        Ok(parser_result)  => {
            if CLI_ARGS.show_ast {
                println!("AST:\n{:#?}", &parser_result);
            } 
            generate_code(&parser_result.unit);
        }
        Err(e) => {
            eprintln!("Parse error!");
            match e {
                Error::SyntaxError(syntax_error) => {
                    eprintln!("Source code:");
                    for (idx, line) in syntax_error.source.lines().enumerate() {
                        eprintln!("{:4}  {}", idx + 1, line);
                    }
                    eprintln!("At line {}, column {}", syntax_error.line, syntax_error.column);
                }
                _ => {
                    eprintln!("{:#?}", e);
                }
            }
        }
    }
}

