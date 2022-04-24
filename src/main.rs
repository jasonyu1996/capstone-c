#![feature(box_patterns, once_cell)]

use std::any::type_name;
use std::fmt::Display;
use std::os::unix::process::parent_id;
/**
 * Features omitted: no distinguishing different int sizes
 *
 * */


use std::{iter::Map, collections::HashMap};
use std::str::FromStr;
use std::lazy::Lazy;

use lang_c::driver::{SyntaxError, Error};
use lang_c::{driver::{Config, parse}, ast::{TranslationUnit, FunctionDefinition,
    StaticAssert, Declarator, Declaration, DeclaratorKind, Statement, BlockItem, InitDeclarator, Integer, Initializer, Expression, Constant}};
use lang_c::ast::{ExternalDeclaration, BinaryOperatorExpression, BinaryOperator, Identifier, IfStatement, WhileStatement, CallExpression, AsmStatement, UnaryOperatorExpression, UnaryOperator, MemberExpression, MemberOperator, DeclarationSpecifier, TypeSpecifier, StructType, StructKind, StructDeclaration, StructField, SpecifierQualifier, DerivedDeclarator, FunctionDeclarator, ParameterDeclaration, StructDeclarator, Extension, ArraySize, TypeName};
use lang_c::span::Node;
use clap::Parser as ClapParser;

const TEECAP_GPR_N: usize = 32; // number of general-purpose registers
const TEECAP_STACK_REG: TeecapReg = TeecapReg::Gpr(0); // r0 is used for storing the stack capability
const TEECAP_SEALED_REGION_SIZE: usize = TEECAP_GPR_N + 4;

const TEECAP_SEALED_OFFSET_PC: usize = 0;
const TEECAP_SEALED_OFFSET_STACK_REG: usize = 4;

type TeecapInt = u64;

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

const CLI_ARGS: Lazy<Args> = Lazy::new(|| {
    Args::parse()
});


#[derive(Clone, Copy, Debug)]
enum TeecapRegState {
    Pinned,
    Grabbed,
    Free
}

impl TeecapRegState {
    fn grab(&mut self) {
        if !matches!(*self, TeecapRegState::Pinned) {
            *self = TeecapRegState::Grabbed;
        }
    }

    fn free(&mut self) {
        if !matches!(*self, TeecapRegState::Pinned) {
            *self = TeecapRegState::Free;
        }
    }

    fn pin(&mut self) {
        *self = TeecapRegState::Pinned;
    }

    fn reset(&mut self) {
        *self = TeecapRegState::Free;
    }

    fn is_available(&self) -> bool {
        matches!(*self, TeecapRegState::Free)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum TeecapReg {
    Pc, Sc, Epc, Ret, Gpr(u8)
}

impl Display for TeecapReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
           TeecapReg::Pc => {
               write!(f, "pc")
           }
           TeecapReg::Sc => {
               write!(f, "sc")
           }
           TeecapReg::Epc => {
               write!(f, "epc")
           }
           TeecapReg::Ret => {
               write!(f, "ret")
           }
           TeecapReg::Gpr(n) => {
               write!(f, "r{}", n)
           }
        }
    }
}

#[derive(Clone, Debug)]
enum TeecapImm {
    NTag(TeecapNTag, bool), // bool: relative
    STag(TeecapSTag, bool),
    Const(TeecapInt)
}

#[derive(Clone)]
enum TeecapInsn {
    Li(TeecapReg, TeecapImm),
    Sd(TeecapReg, TeecapReg),
    Ld(TeecapReg, TeecapReg),
    Scc(TeecapReg, TeecapReg),
    Scco(TeecapReg, TeecapReg),
    Lcc(TeecapReg, TeecapReg),
    Lcco(TeecapReg, TeecapReg),
    Lcb(TeecapReg, TeecapReg),
    Lce(TeecapReg, TeecapReg),
    Lcn(TeecapReg, TeecapReg),
    Add(TeecapReg, TeecapReg),
    Sub(TeecapReg, TeecapReg),
    Mult(TeecapReg, TeecapReg),
    Div(TeecapReg, TeecapReg),
    Jnz(TeecapReg, TeecapReg),
    Jz(TeecapReg, TeecapReg),
    Eq(TeecapReg, TeecapReg, TeecapReg),
    Le(TeecapReg, TeecapReg, TeecapReg),
    Lt(TeecapReg, TeecapReg, TeecapReg),
    And(TeecapReg, TeecapReg),
    Split(TeecapReg, TeecapReg, TeecapReg),
    Splitl(TeecapReg, TeecapReg, TeecapReg),
    Splito(TeecapReg, TeecapReg, TeecapReg),
    Splitlo(TeecapReg, TeecapReg, TeecapReg),
    Mov(TeecapReg, TeecapReg),
    Or(TeecapReg, TeecapReg),
    Mrev(TeecapReg, TeecapReg),
    Call(TeecapReg, TeecapReg),
    Lin(TeecapReg),
    Delin(TeecapReg),
    Tighten(TeecapReg, TeecapReg),
    Shrink(TeecapReg, TeecapReg, TeecapReg),
    Seal(TeecapReg),
    SealRet(TeecapReg, TeecapReg),
    Drop(TeecapReg),
    Ret(TeecapReg, TeecapReg),
    RetSealed(TeecapReg, TeecapReg),
    Jmp(TeecapReg),
    Out(TeecapReg),
    Halt
}

impl TeecapReg {
    fn try_from_str(s: &str) -> Option<TeecapReg> {
        if s.starts_with('r') && !s.starts_with("re") {
            Some(TeecapReg::Gpr(u8::from_str(&s[1..]).ok()?))
        } else{
            match s {
                "pc" => Some(TeecapReg::Pc),
                "ret" => Some(TeecapReg::Ret),
                "sc" => Some(TeecapReg::Sc),
                "epc" => Some(TeecapReg::Epc),
                _ => None
            }
        }
    }
}


#[derive(Debug)]
struct TeecapFunctionType {
    pinned_gprs: Vec<TeecapReg>
}

impl Default for TeecapFunctionType {
    fn default() -> TeecapFunctionType {
        TeecapFunctionType { pinned_gprs: Vec::new() }
    }
}


impl TeecapFunctionType {
    fn from_specifiers(specifiers: &Vec<Node<DeclarationSpecifier>>) -> TeecapFunctionType {
        let mut func_type = TeecapFunctionType::default();
        for spec in specifiers.iter() {
            match &spec.node {
                DeclarationSpecifier::Extension(extensions) => {
                    for ext in extensions.iter() {
                        match &ext.node {
                            Extension::Attribute(attr) => {
                                match attr.name.node.as_str() {
                                    "pinned" => {
                                        func_type.pinned_gprs.extend(attr.arguments.iter().filter_map(|x| x.node.teecap_try_into_str().and_then(|s| TeecapReg::try_from_str(s.trim_matches('\"')))));
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

impl Display for TeecapImm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapImm::NTag(tag, false) => {
                tag.fmt(f)
            }
            TeecapImm::STag(tag, false) => {
                tag.fmt(f)
            }
            TeecapImm::NTag(tag, true) => {
                write!(f, "*{}", tag)
            }
            TeecapImm::STag(tag, true) => {
                write!(f, "*{}", tag)
            }
            TeecapImm::Const(c) => {
                c.fmt(f)
            }
        }
    }
}

#[derive(Clone, Debug)]
struct TeecapRegResult {
    reg: TeecapReg,
    release_strategy: TeecapRegRelease,
    data_type: TeecapType
}

impl TeecapRegResult {
    fn new_simple(reg: TeecapReg, dtype: TeecapType) -> TeecapRegResult {
        TeecapRegResult { reg: reg, release_strategy: TeecapRegRelease::Simple, data_type: dtype}
    }

    fn new_writeback(reg: TeecapReg, wb_to: TeecapRegResult, offset: TeecapOffset, dtype: TeecapType) -> TeecapRegResult {
        TeecapRegResult { reg: reg, release_strategy: TeecapRegRelease::WriteBack(Box::new(wb_to), offset), data_type: dtype }
    }

    fn set_writeback(&mut self, wb_to: TeecapRegResult, offset: TeecapOffset) {
        self.release_strategy = TeecapRegRelease::WriteBack(Box::new(wb_to), offset);
    }

    fn set_type(&mut self, data_type: TeecapType) {
        self.data_type = data_type;
    }

    fn to_simple(&self) -> TeecapRegResult {
        TeecapRegResult { reg: self.reg, release_strategy: TeecapRegRelease::Simple, data_type: self.data_type.clone() }
    }
}

#[derive(Clone, Debug)]
enum TeecapRegRelease {
    Simple,
    WriteBack(Box<TeecapRegResult>, TeecapOffset) // the reliant capability register and the offset
}

impl Display for TeecapInsn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapInsn::Li(r, v) => {
                write!(f, "li {} {}", r, v)
            }
            TeecapInsn::Sd(rd, rs) => {
                write!(f, "sd {} {}", rd, rs)
            }
            TeecapInsn::Ld(rd, rs) => {
                write!(f, "ld {} {}", rd, rs)
            }
            TeecapInsn::Scc(rd, rs) => {
                write!(f, "scc {} {}", rd, rs)
            }
            TeecapInsn::Scco(rd, rs) => {
                write!(f, "scco {} {}", rd, rs)
            }
            TeecapInsn::Lcc(rd, rs) => {
                write!(f, "lcc {} {}", rd, rs)
            }
            TeecapInsn::Lcco(rd, rs) => {
                write!(f, "lcco {} {}", rd, rs)
            }
            TeecapInsn::Add(rd, rs) => {
                write!(f, "add {} {}", rd, rs)
            }
            TeecapInsn::Sub(rd, rs) => {
                write!(f, "sub {} {}", rd, rs)
            }
            TeecapInsn::Mult(rd, rs) => {
                write!(f, "mult {} {}", rd, rs)
            }
            TeecapInsn::Div(rd, rs) => {
                write!(f, "div {} {}", rd, rs)
            }
            TeecapInsn::Jnz(rd, rs) => {
                write!(f, "jnz {} {}", rd, rs)
            }
            TeecapInsn::Jz(rd, rs) => {
                write!(f, "jz {} {}", rd, rs)
            }
            TeecapInsn::Jmp(r) => {
                write!(f, "jmp {}", r)
            }
            TeecapInsn::Out(r) => {
                write!(f, "out {}", r)
            }
            TeecapInsn::Halt => {
                write!(f, "halt")
            }
            TeecapInsn::Le(rd, r1, r2) => {
                write!(f, "le {} {} {}", rd, r1, r2)
            }
            TeecapInsn::Lt(rd, r1, r2) => {
                write!(f, "lt {} {} {}", rd, r1, r2)
            }
            TeecapInsn::Eq(rd, r1, r2) => {
                write!(f, "eq {} {} {}", rd, r1, r2)
            }
            TeecapInsn::And(rd, rs) => {
                write!(f, "and {} {}", rd, rs)
            }
            TeecapInsn::Or(rd, rs) => {
                write!(f, "or {} {}", rd, rs)
            }
            TeecapInsn::Split(rd, rs, rp) => {
                write!(f, "split {} {} {}", rd, rs, rp)
            }
            TeecapInsn::Splito(rd, rs, rp) => {
                write!(f, "splito {} {} {}", rd, rs, rp)
            }
            TeecapInsn::Splitl(rd, rs, rp) => {
                write!(f, "splitl {} {} {}", rd, rs, rp)
            }
            TeecapInsn::Splitlo(rd, rs, rp) => {
                write!(f, "splitlo {} {} {}", rd, rs, rp)
            }
            TeecapInsn::Lcb(rd, rs) => {
                write!(f, "lcb {} {}", rd, rs)
            }
            TeecapInsn::Lce(rd, rs) => {
                write!(f, "lce {} {}", rd, rs)
            }
            TeecapInsn::Lcn(rd, rs) => {
                write!(f, "lcn {} {}", rd, rs)
            }
            TeecapInsn::Mrev(rd, rs) => {
                write!(f, "mrev {} {}", rd, rs)
            }
            TeecapInsn::Mov(rd, rs) => {
                write!(f, "mov {} {}", rd, rs)
            }
            TeecapInsn::Seal(r) => {
                write!(f, "seal {}", r)
            }
            TeecapInsn::SealRet(rd, rs) => {
                write!(f, "sealret {} {}", rd, rs)
            }
            TeecapInsn::Call(r, ra) => {
                write!(f, "call {} {}", r, ra)
            }
            TeecapInsn::Lin(r) => {
                write!(f, "lin {}", r)
            }
            TeecapInsn::Delin(r) => {
                write!(f, "delin {}", r)
            }
            TeecapInsn::Tighten(rd, rs) => {
                write!(f, "tighten {} {}", rd, rs)
            }
            TeecapInsn::Shrink(rd, rb, re) => {
                write!(f, "shrink {} {} {}", rd, rb, re)
            }
            TeecapInsn::Drop(r) => {
                write!(f, "drop {}", r)
            }
            TeecapInsn::Ret(rd, rs) => {
                write!(f, "ret {} {}", rd, rs)
            }
            TeecapInsn::RetSealed(rd, rs) => {
                write!(f, "retsl {} {}", rd, rs)
            }
            _ => {
                write!(f, "<und>")
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TeecapNTag(u32);

#[derive(Clone, Debug)]
struct TeecapSTag(String);

enum TeecapAssemblyUnit {
    Tag(TeecapNTag),
    Insn(TeecapInsn),
    Passthrough(String),
    Comment(String)
}

impl Display for TeecapSTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":<{}>", self.0)
    }
}

impl Display for TeecapNTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":<{}>", self.0)
    }
}

impl TeecapAssemblyUnit {
    fn print_code(&self, mem_offset: &mut u32) {
        match &self {
            TeecapAssemblyUnit::Tag(tag) => {
                println!("{}", tag);
            }
            TeecapAssemblyUnit::Insn(insn) => {
                println!("{}", insn);
                *mem_offset += 1;
            }
            TeecapAssemblyUnit::Passthrough(s) => {
                println!("{}", s);
                *mem_offset += 1;
            }
            TeecapAssemblyUnit::Comment(s) => {
                if CLI_ARGS.print_comments {
                    println!("# {}", s);
                }
            }
        }
    }
}

type TeecapFunctionGroupId = u32;


struct TeecapFunction {
    group: TeecapFunctionGroupId,
    name: String, // name of the function
    code: Vec<TeecapAssemblyUnit>,
    func_type: TeecapFunctionType,
    return_type: TeecapType
}

impl TeecapFunction {
    fn new(name: &str, func_type: TeecapFunctionType, return_type: TeecapType) -> TeecapFunction {
        TeecapFunction {
            group: 0,
            name: name.to_string(),
            code: Vec::new(),
            func_type,
            return_type
        }
    }

    fn push_asm_unit(&mut self, asm_unit: TeecapAssemblyUnit) {
        self.code.push(asm_unit);
    }

    fn print_code(&self, mem_offset: &mut u32) {
        println!("{} {}", mem_offset, -1);
        println!(":<{}>", self.name);
        for asm_unit in self.code.iter() {
            asm_unit.print_code(mem_offset);
        }
        println!("$");
    }
}

#[derive(Debug, Clone)]
struct TeecapVariable {
    offset_in_cap: TeecapOffset, // offset inside the capability
    ttype: TeecapType,
    //parent_type: Option<String>,
    //name: String,
    cap: TeecapRegResult, // actuall register that has the capability for loading the variable
}

#[derive(Debug, Clone)]
struct TeecapUnresolvedVar(String); // this is the unresolved variable. We don't know where to find it yet. We know nothing more than the name
    // we are assuming that the member operators are left associative

impl TeecapVariable {
    fn join_indirect(&self, other: &TeecapUnresolvedVar, ctx: &mut CodeGenContext) -> Option<TeecapEvalResult> {
        //eprintln!("Join {:?}", self.ttype);
        match &self.ttype {
            TeecapType::Cap(base_type) => { // indirect join can only be performed on a capability
                match base_type {
                    Some(box TeecapType::Struct(base_struct_name)) => {
                        let reg_lhs = ctx.gen_ld_alloc(self); // holds a capability
                        //ctx.push_insn(TeecapInsn::Out(reg_lhs.reg));
                        let (offset, ttype) = ctx.resolve_var(TeecapOffset::Const(0), &Some(base_struct_name.to_string()), other)?;
                        //eprintln!("Resolved: {:?}", (offset, &ttype));
                        Some(TeecapEvalResult::Variable(TeecapVariable {
                            offset_in_cap: offset,
                            ttype: ttype,
                            cap: reg_lhs.clone()
                        }))
                    } 
                    _ => None
                }
            }
            _ => None
        }
    }
    fn join_direct(&self, other: &TeecapUnresolvedVar, ctx: &mut CodeGenContext) -> Option<TeecapEvalResult> {
        match &self.ttype {
            TeecapType::Cap(_) => {
                match other.0.as_str() {
                    "cursor" => {
                        ctx.gen_cap_query(self, TeecapInsn::Lcc)
                    }
                    "offset" => {
                        ctx.gen_cap_query(self, TeecapInsn::Lcco)
                    }
                    "base" => {
                        ctx.gen_cap_query(self, TeecapInsn::Lcb)
                    }
                    "end" => {
                        ctx.gen_cap_query(self, TeecapInsn::Lce)
                    }
                    "size" => {
                        ctx.gen_cap_query(self, TeecapInsn::Lcn)
                    }
                    _ => None
                }
            }
            TeecapType::Struct(parent_name) => {
                let (offset, ttype) = ctx.resolve_var(self.offset_in_cap, &Some(parent_name.to_string()), other).expect("Field not found!");
                Some(TeecapEvalResult::Variable(TeecapVariable { offset_in_cap: offset, 
                    ttype: ttype,
                    cap: self.cap.clone()}))
            }
            _ => {
                None
            }
        }
    }
}

type TeecapScope = HashMap<String, (u32, TeecapField)>;

#[derive(Debug, Clone)]
enum TeecapEvalResult {
    Const(TeecapInt),
    Register(TeecapRegResult),
    UnresolvedVar(TeecapUnresolvedVar),
    Variable(TeecapVariable) // parent offset, parent type, name
}

impl TeecapEvalResult {
    fn get_type(&self) -> &TeecapType {
        match self {
            TeecapEvalResult::Const(_) => &TeecapType::Int,
            TeecapEvalResult::Register(reg) => &reg.data_type,
            TeecapEvalResult::Variable(var) => &var.ttype,
            TeecapEvalResult::UnresolvedVar(var) => &TeecapType::Int, // resolve first, otherwise fall back to int
        }
    }

    fn try_into_variable(&self, ctx: &mut CodeGenContext) -> Option<TeecapVariable> {
        match self {
            TeecapEvalResult::Variable(var) => Some(var.clone()),
            TeecapEvalResult::UnresolvedVar(var) => {
                ctx.resolve_top_var(var)
            },
            _ => None
        }
    }


    fn resolve(&mut self, ctx: &mut CodeGenContext) {
        if let TeecapEvalResult::UnresolvedVar(var) = self {
            *self = TeecapEvalResult::Variable(ctx.resolve_top_var(&var).expect("Failed to resolve variable!"));
        }
    }

    fn to_register(&self, ctx: &mut CodeGenContext) -> TeecapRegResult {
        self.to_register_with_offset(0, ctx).unwrap() // should not fail at offset 0
    }

    fn to_register_with_offset(&self, offset: u32, ctx: &mut CodeGenContext) -> Option<TeecapRegResult> {
        if self.get_size(ctx) <= offset {
            //eprintln!("{:?} {}", self, self.get_size(ctx));
            return None;
        }
        match self {
            &TeecapEvalResult::Const(n) => {
                Some(ctx.gen_li_alloc(TeecapImm::Const(n)))
            }
            TeecapEvalResult::Register(r) => {
                Some(r.clone())
            }
            TeecapEvalResult::Variable(var) => {
                // load the variable
                Some(ctx.gen_ld_alloc_extra_offset(var, &TeecapOffset::Const(offset))) // note: here the type and size 
                    // information will be lost
            }
            TeecapEvalResult::UnresolvedVar(unresolved_var) => {
                let r = ctx.resolve_top_var(unresolved_var).map(|x| ctx.gen_ld_alloc_extra_offset(&x, &TeecapOffset::Const(offset)));
                if r.is_some() {
                    r
                } else{
                    Some(ctx.gen_li_alloc(TeecapImm::STag(TeecapSTag(unresolved_var.0.clone()), false)))
                }
            }
        }
    }
    fn join_indirect(&self, other: &TeecapEvalResult, ctx: &mut CodeGenContext) -> Option<TeecapEvalResult> {
        if let TeecapEvalResult::UnresolvedVar(rhs) = other { // the rhs can only be an unresolved variable
            match self {
                TeecapEvalResult::Variable(var) => {
                    var.join_indirect(rhs, ctx)
                }
                TeecapEvalResult::UnresolvedVar(var) => {
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
    fn join_direct(&self, other: &TeecapEvalResult, ctx: &mut CodeGenContext) -> Option<TeecapEvalResult> {
        if let TeecapEvalResult::UnresolvedVar(rhs) = other {
            //eprintln!("{:#?}", self);
            match self {
                TeecapEvalResult::Variable(var) => {
                    var.join_direct(rhs, ctx)
                }
                TeecapEvalResult::UnresolvedVar(var) => {
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
            TeecapEvalResult::Const(_) => 1,
            TeecapEvalResult::Register(_) => 1,
            TeecapEvalResult::Variable(var) => {
                //eprintln!("{:#?}", (offset, parent, name, ctx.variables.first().unwrap(), &ctx.struct_map));
                var.ttype.get_size(&ctx.struct_map)
            }
            TeecapEvalResult::UnresolvedVar(var) => {
                ctx.resolve_top_var(var).map(|x| x.ttype.get_size(&ctx.struct_map)).unwrap_or(1)
            }
        }
    }
}

//struct TeecapContext {}

trait TeecapEvaluator {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult;
}

trait TeecapEmitter {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext);
}

trait TeecapLiteralConstruct<T> {
    fn to_teecap_literal(&self) -> T;
}

impl TeecapLiteralConstruct<TeecapInt> for Integer {
    fn to_teecap_literal(&self) -> TeecapInt {
        // ignoring suffix and base for now
        TeecapInt::from_str(&self.number).unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
enum TeecapOffset {
    Const(u32),
    Register(TeecapReg)
}

impl TeecapOffset {
    fn to_register(&self, ctx: &mut CodeGenContext) -> TeecapRegResult {
        match self {
            TeecapOffset::Const(c) => ctx.gen_li_alloc(TeecapImm::Const(*c as u64)),
            TeecapOffset::Register(r) => TeecapRegResult::new_simple(*r, TeecapType::Int)
        }
    }
    fn add(&self, other: &TeecapOffset, ctx: &mut CodeGenContext) -> TeecapOffset {
        match (self, other) {
            // if both are constants known at compile time, we just add the constants together
            (TeecapOffset::Const(c1), TeecapOffset::Const(c2)) => TeecapOffset::Const(c1 + c2),
            _ => {
                // otherwise, we have to compute at runtime
                let r1 = self.to_register(ctx);
                let r2 = other.to_register(ctx);
                ctx.push_insn(TeecapInsn::Add(r1.reg, r2.reg));
                ctx.release_gpr(&r2);
                TeecapOffset::Register(r1.reg)
            }
        }
    }
    fn drop(self, ctx: &mut CodeGenContext) {
        if let TeecapOffset::Register(reg) = self {
            ctx.release_gpr(&TeecapRegResult::new_simple(reg, TeecapType::Int));
        }
    }

    fn from_reg_result(res: &TeecapRegResult, ctx: &mut CodeGenContext) -> TeecapOffset {
        TeecapOffset::Register(res.reg)
    }

    fn duplicate(&self, ctx: &mut CodeGenContext) -> TeecapOffset {
        match self {
            TeecapOffset::Const(n) => TeecapOffset::Const(*n),
            TeecapOffset::Register(reg) =>
                TeecapOffset::Register(ctx.gen_mov_alloc(&TeecapRegResult::new_simple(*reg, TeecapType::Int)).reg)
        }
    }
}

struct CodeGenContext {
    tag_count: u32,
    init_func: TeecapFunction,
    cur_func: Option<TeecapFunction>,
    functions: Vec<TeecapFunction>,
    variables: Vec<TeecapScope>,
    reserved_stack_size: u32,
    gprs: [TeecapRegState; TEECAP_GPR_N],
    break_conts: Vec<(TeecapNTag, TeecapNTag)>, // break and continue stack
    in_func: bool,
    struct_map: TeecapStructMap,
}

#[derive(Copy, Clone, Debug)]
enum TeecapOpRAB {
    Le,
    Lt,
    Ge,
    Gt,
    Eq
}

#[derive(Copy, Clone, Debug)]
enum TeecapOpAB {
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
            variables: vec![TeecapScope::new()],
            reserved_stack_size: 0,
            gprs: [TeecapRegState::Free; TEECAP_GPR_N],
            in_func: false,
            init_func: TeecapFunction::new("_init", TeecapFunctionType::default(), TeecapType::Void),
            break_conts: Vec::new(),
            struct_map: TeecapStructMap::new()
        };
        if let TeecapReg::Gpr(n) = TEECAP_STACK_REG {
            instance.gprs[n as usize] = TeecapRegState::Pinned; // reserve the gpr for the stack cap
        }
        instance
    }

    fn release_gprs(&mut self, gprs: &Vec<TeecapRegResult>) {
        for gpr in gprs.iter() {
            self.release_gpr(&gpr);
        }
    }

    fn gen_index(&mut self, lhs: &mut TeecapEvalResult, rhs: &mut TeecapEvalResult) -> TeecapEvalResult {
        //eprintln!("{:?} {:?}", lhs_reg, rhs_reg);
        //eprintln!("{:?}", self.variables);
        rhs.resolve(self);
        lhs.resolve(self);
        match (lhs.get_type(), rhs.get_type()) {
            (TeecapType::Cap(base_type), TeecapType::Int) => {
                let rhs_reg = rhs.to_register(self); // it probably makes more sense to generate rhs first
                let lhs_reg = lhs.to_register(self);
                let base_size = base_type.as_ref().map_or(1, |t| t.get_size(&self.struct_map));
                //eprintln!("base_size = {}", base_size);
                let rsize = self.gen_li_alloc(TeecapImm::Const(base_size as u64));
                self.push_insn(TeecapInsn::Mult(rsize.reg, rhs_reg.reg));
                self.release_gpr(&rhs_reg);
                TeecapEvalResult::Variable(TeecapVariable { 
                    offset_in_cap: TeecapOffset::from_reg_result(&rsize, self),
                    ttype: base_type.as_ref().map_or(TeecapType::Int, |x| (**x).clone()),
                    cap: lhs_reg
                })
            }
            (TeecapType::Array(base_type, _len), TeecapType::Int) => {
                let lhs_var = lhs.try_into_variable(self).expect("Bad index op!");
                let rhs_reg = rhs.to_register(self);
                //eprintln!("{:?} {:?} {:?}", lhs_var, rhs_reg, base_type);
                let base_size = base_type.get_size(&self.struct_map);
                let rsize = self.gen_li_alloc(TeecapImm::Const(base_size as u64));
                self.push_insn(TeecapInsn::Mult(rsize.reg, rhs_reg.reg));
                self.release_gpr(&rhs_reg);
                TeecapEvalResult::Variable(TeecapVariable {
                    offset_in_cap: lhs_var.offset_in_cap.add(&TeecapOffset::Register(rsize.reg), self),
                    ttype: *base_type.clone(),
                    cap: lhs_var.cap.clone()
                })
            }
            _ => {
                TeecapEvalResult::Const(0)
            }
        }
    }

    fn get_cur_func_group_id(&self) -> TeecapFunctionGroupId {
        self.cur_func.as_ref().expect("Failed to obtain current function group ID (not in function context)")
            .group
    }

    fn get_cur_func_name(&self) -> &str {
        &self.cur_func.as_ref().expect("Failed to obtain current function group ID (not in function context)")
            .name
    }

    fn add_var_to_scope(&mut self, field: TeecapField) {
        let offset = self.reserved_stack_size;
        self.reserved_stack_size += field.get_size(&self.struct_map);
        self.variables.first_mut().unwrap().insert(field.name.clone(), (offset, field));
    }

    fn find_var_in_scope(&self, var_name: &str) -> Option<(u32, TeecapType)> {
        self.variables.iter().map(|x| x.get(var_name)).fold(None, |x, y| x.or(y.map(|v| (v.0, v.1.field_type.clone()))))
    }

    fn pop_scope(&mut self) {
        self.variables.pop();
    }

    fn push_scope(&mut self) {
        self.variables.push(TeecapScope::new());
    }

    fn push_asm_unit(&mut self, asm_unit: TeecapAssemblyUnit) {
        if self.in_func {
            self.cur_func.as_mut().expect("Function list is empty!")
        } else {
            &mut self.init_func
        }.push_asm_unit(asm_unit);
    }

    fn enter_function(&mut self, func_name: &str, func_type: TeecapFunctionType, func_return_type: TeecapType) {
        self.in_func = true;
        self.reserved_stack_size = 0;
        self.reset_gprs();
        for pinned_gpr in func_type.pinned_gprs.iter() {
            self.pin_gpr(*pinned_gpr);
        }
        self.cur_func = Some(TeecapFunction::new(func_name, func_type, func_return_type));
        

        // not really necessary to do this
        //let r = self.gen_li_alloc(TeecapImm::Const(0));
        //self.push_insn(TeecapInsn::Scco(TEECAP_STACK_REG, r.reg));
        //self.release_gpr(&r);
    }

    fn exit_function(&mut self) {
        self.gen_return(&TeecapEvalResult::Const(0), &TeecapEvalResult::Const(0));
        self.reset_gprs();

        let cur_func = std::mem::take(&mut self.cur_func).expect("Error exit_function: current function not found!");
        self.functions.push(cur_func);
        self.in_func = false;
    }

    fn grab_gpr(&mut self, reg: TeecapReg) {
        if let TeecapReg::Gpr(n) = reg {
            self.gprs[n as usize].grab();
        }
    }

    fn grab_new_gpr(&mut self) -> Option<TeecapRegResult> {
        let some_n = self.gprs.iter().enumerate().find(|&(idx, &b)| b.is_available()).map(|x| x.0 as u8);
        if let &Some(n) = &some_n {
            self.gprs[n as usize].grab();
        }
        some_n.map(|x| TeecapRegResult::new_simple(TeecapReg::Gpr(x), TeecapType::Int))
    }

    fn get_gpr_state(&self, reg: TeecapReg) -> TeecapRegState {
        match reg {
            TeecapReg::Gpr(n) => self.gprs[n as usize],
            _ => TeecapRegState::Pinned
        }
    }

    fn release_gpr(&mut self, reg: &TeecapRegResult) {
        match &reg.release_strategy {
            TeecapRegRelease::Simple => {
                // no need to do anything
                self.release_gpr_no_recurse(reg);
            }
            TeecapRegRelease::WriteBack(wb_to, offset) => {
                self.gen_store_with_cap(offset, wb_to, &TeecapEvalResult::Register(reg.to_simple()));
                self.release_gpr_no_recurse(reg);
                offset.drop(self);
                //self.push_insn(TeecapInsn::Sd(wb_to.reg, reg.reg));
                self.release_gpr(wb_to); // TODO: change to loops
            }
        }
    }

    fn release_gpr_ancestors(&mut self, reg: &TeecapRegResult) {
        match &reg.release_strategy {
            TeecapRegRelease::WriteBack(wb_to, _) => {
                self.release_gpr(wb_to);
            }
            _ => { }
        }
    }

    fn release_gpr_no_recurse(&mut self, reg: &TeecapRegResult) {
        if reg.reg != TEECAP_STACK_REG {
            if let &TeecapReg::Gpr(n) = &reg.reg {
                self.gprs[n as usize].free();
            }
        }
    }

    fn pin_gpr(&mut self, reg: TeecapReg) {
        if let TeecapReg::Gpr(n) = reg {
            self.gprs[n as usize].pin();
        }
    }

    fn reset_gprs(&mut self) {
        self.gprs.iter_mut().for_each(|x| x.reset());
        self.pin_gpr(TEECAP_STACK_REG);
    }

    fn gen_li(&mut self, reg: &TeecapRegResult, val: TeecapImm) {
        self.push_insn(TeecapInsn::Li(reg.reg,
            val));
    }

    fn gen_li_alloc(&mut self, val: TeecapImm) -> TeecapRegResult {
        let reg = self.grab_new_gpr().expect("Gpr allocation for li failed!");
        self.gen_li(&reg, val);
        reg
    }

    fn gen_ld_extra_offset(&mut self, reg: &mut TeecapRegResult, var: &TeecapVariable, offset: &TeecapOffset) {
        if var.ttype.is_cap() {
            let dup_offset = var.offset_in_cap.duplicate(self);
            let cap_reg = self.gen_load_cap_extra_offset(var, offset).expect("Failed to obtain capability for ld!");
            self.push_insn(TeecapInsn::Ld(reg.reg, cap_reg.reg));
            reg.set_writeback(cap_reg, dup_offset.add(&offset, self));
        } else{
            let cap_reg = self.gen_load_cap_extra_offset(var, offset).expect("Failed to obtain capability for ld!");
            self.push_insn(TeecapInsn::Ld(reg.reg, cap_reg.reg));
            self.release_gpr(&cap_reg); // TODO: we should not reuse cap registers like this
        }
        reg.set_type(var.ttype.clone());
        // it is necessary to write them back if the capability is linear
    }

    fn gen_ld_alloc_extra_offset(&mut self, var: &TeecapVariable, offset: &TeecapOffset) -> TeecapRegResult {
        let mut reg = self.grab_new_gpr().expect("Gpr allocation for ld failed!");
        self.gen_ld_extra_offset(&mut reg, var, offset);
        reg
    }

    fn gen_ld_alloc(&mut self, var: &TeecapVariable) -> TeecapRegResult {
        self.gen_ld_alloc_extra_offset(var, &TeecapOffset::Const(0))
    }

    // the var is stored on stack
    fn resolve_top_var(&mut self, var: &TeecapUnresolvedVar) -> Option<TeecapVariable> {
        let (offset, ttype) = self.resolve_var(TeecapOffset::Const(0), &None, var)?;
        Some(TeecapVariable {
            offset_in_cap: offset,
            ttype,
            cap: TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None))
        })
    }

    fn resolve_var(&mut self, offset: TeecapOffset, parent_struct: &Option<String>, var: &TeecapUnresolvedVar) -> Option<(TeecapOffset, TeecapType)> {
        match parent_struct {
            Some(parent_name) => {
                let par = self.struct_map.get(parent_name)?;
                //eprintln!("{:#?}", par);
                par.get_field(&var.0).map(|(o, t)| (offset.add(&TeecapOffset::Const(o), self), t))
            }
            None => {
                self.find_var_in_scope(&var.0).map(|(o, t)| (offset.add(&TeecapOffset::Const(o), self), t))
            }
        }
    }

    fn gen_load_cap_extra_offset(&mut self, var: &TeecapVariable, extra_offset: &TeecapOffset) -> Option<TeecapRegResult> {
        let offset_reg = var.offset_in_cap.add(extra_offset, self).to_register(self);
        let cap_reg = var.cap.reg; // TODO: change this to a TeecapRegResult to keep track of writeback
        self.push_insn(TeecapInsn::Scco(cap_reg, offset_reg.reg));
        self.release_gpr(&offset_reg);
        // TODO: handle writeback here
        Some(var.cap.clone())
    }

    /**
     * Returns: the register that contains the capability which can be used for accessing
     * the specified variable (might fail if the variable cannot be accessed).
     * */
    fn gen_load_cap(&mut self, var: &TeecapVariable) -> Option<TeecapRegResult> {
        self.gen_load_cap_extra_offset(var, &TeecapOffset::Const(0))
    }

    fn gen_store_with_cap(&mut self, offset: &TeecapOffset, rcap: &TeecapRegResult, val: &TeecapEvalResult) {
        let rvalue_size = val.get_size(self);
        for i in 0..rvalue_size {
            let reg = val.to_register_with_offset(i, self).unwrap();
            self.release_gpr_ancestors(&reg); // since there is no need to write the value back, we release the ancestors in advance
            //self.push_insn(TeecapInsn::Out(TeecapReg::Pc));
            //self.push_insn(TeecapInsn::Out(reg.reg));
            let noffset = &offset.add(&TeecapOffset::Const(i), self);
            self.gen_sd_imm_offset(rcap, &reg, &noffset);
            self.release_gpr_no_recurse(&reg);
        }
        //self.push_insn(TeecapInsn::Out(TeecapReg::Epc));
    }

    fn gen_store(&mut self, var: &TeecapVariable, val: &TeecapEvalResult) -> TeecapEvalResult {
        let rvalue_size = val.get_size(self);
        for i in 0..rvalue_size {
            let reg = val.to_register_with_offset(i, self).unwrap();
            let cap_reg = self.gen_load_cap_extra_offset(var, &TeecapOffset::Const(i)).expect("Unable to access variable through capabilities!");
            self.push_insn(TeecapInsn::Sd(cap_reg.reg, reg.reg));
            self.release_gpr(&reg);
            self.release_gpr(&cap_reg);
        }
        val.clone()
    }

    fn gen_store_drop_result(&mut self, var: &TeecapVariable, val: &TeecapEvalResult) {
        let res = self.gen_store(var, val);
        self.drop_result(&res);
    }

    fn gen_assignment(&mut self, lhs: &TeecapEvalResult, rhs: &TeecapEvalResult) -> TeecapEvalResult {
        match &lhs {
            TeecapEvalResult::Variable(var) => {
                self.gen_store(var, rhs)
            }
            TeecapEvalResult::UnresolvedVar(var) => {
                let var = self.resolve_top_var(var).expect("Failed to resolve variable for assignment!");
                self.gen_store(&var, rhs)
            }
            TeecapEvalResult::Register(reg) => {
                // this can happen in reg()
                let rhs_reg = rhs.to_register(self);
                self.push_insn(TeecapInsn::Mov(reg.reg, rhs_reg.reg)); // then we simply move the value and we are done
                self.release_gpr(&rhs_reg);
                lhs.clone()
            }
            _ => {
                eprintln!("Unsupported lvalue for assignment: {:?}", lhs);
                TeecapEvalResult::Const(0)
            }
        }
    }

    fn gen_a_b(&mut self, op: TeecapOpAB, lhs: &TeecapEvalResult, rhs: &TeecapEvalResult) -> TeecapEvalResult {
        let rd = lhs.to_register(self);
        let rs = rhs.to_register(self);
        self.push_insn(match op {
            TeecapOpAB::Plus => {
                TeecapInsn::Add(rd.reg, rs.reg)
            }
            TeecapOpAB::Minus => {
                TeecapInsn::Sub(rd.reg, rs.reg)
            }
            TeecapOpAB::And => {
                TeecapInsn::And(rd.reg, rs.reg)
            }
            TeecapOpAB::Mult => {
                TeecapInsn::Mult(rd.reg, rs.reg)
            }
            TeecapOpAB::Div => {
                TeecapInsn::Div(rd.reg, rs.reg)
            }
            TeecapOpAB::Or => {
                TeecapInsn::Or(rd.reg, rs.reg)
            }
        });
        self.release_gpr(&rs);
        TeecapEvalResult::Register(rd)
    }

    fn print_code(&self) {
        println!("{} {} {} {} {}", CLI_ARGS.mem_size, CLI_ARGS.gpr_n, CLI_ARGS.clock_rate, 1, -1);
        println!("{} {} {}", 0, ":<stack>", ":<_init>"); // the first is the pc
        println!("{} {} {}", ":<stack>", CLI_ARGS.mem_size, ":<stack>");
        let mut mem_offset = 0;
        self.init_func.print_code(&mut mem_offset);
        for func in self.functions.iter() {
            func.print_code(&mut mem_offset);
        }
        // stack
        println!("{} {}", mem_offset, -1);
        println!(":<stack>");
        println!("$");
        //mem_offset += TEECAP_STACK_SIZE;
        //println!("{} {}", mem_offset, -1);
        //println!(":<heap>");
        //println!("$");
        println!("-1 -1");
    }

    fn drop_result(&mut self, res: &TeecapEvalResult) {
        if let TeecapEvalResult::Register(reg) = res {
            self.release_gpr(&reg);
        }
    }

    fn alloc_tag(&mut self) -> TeecapNTag {
        let tag = TeecapNTag(self.tag_count);
        self.tag_count += 1;
        tag
    }
    
    fn push_break_cont(&mut self, break_tag: TeecapNTag, cont_tag: TeecapNTag) {
        self.break_conts.push((break_tag, cont_tag));
    }

    fn pop_break_cont(&mut self) {
        self.break_conts.pop();
    }
    
    fn gen_jmp_tag(&mut self, tag: TeecapNTag) {
        let target_reg = self.gen_li_alloc(TeecapImm::NTag(tag, true));
        self.push_insn(TeecapInsn::Jmp(target_reg.reg));
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

    fn add_struct(&mut self, teecap_struct: TeecapStruct) {
        self.struct_map.insert(teecap_struct.name.clone(), teecap_struct);
    }


    fn gen_r_a_b(&mut self, op: TeecapOpRAB, lhs: &TeecapEvalResult, rhs: &TeecapEvalResult) -> TeecapEvalResult {
        let lhs_reg = lhs.to_register(self);
        let rhs_reg = rhs.to_register(self);
        let res_reg = self.grab_new_gpr().expect("Failed to allocate gpr for eq!");
        self.push_insn(match op {
            TeecapOpRAB::Le => {
                TeecapInsn::Le(res_reg.reg, lhs_reg.reg, rhs_reg.reg)
            }
            TeecapOpRAB::Lt => {
                TeecapInsn::Lt(res_reg.reg, lhs_reg.reg, rhs_reg.reg)
            }
            TeecapOpRAB::Ge => {
                TeecapInsn::Le(res_reg.reg, rhs_reg.reg, lhs_reg.reg)
            }
            TeecapOpRAB::Gt => {
                TeecapInsn::Lt(res_reg.reg, rhs_reg.reg, lhs_reg.reg)
            }
            TeecapOpRAB::Eq => {
                TeecapInsn::Eq(res_reg.reg, lhs_reg.reg, rhs_reg.reg)
            }
        });
        self.release_gpr(&lhs_reg);
        self.release_gpr(&rhs_reg);
        TeecapEvalResult::Register(res_reg)
    }

    fn gen_not(&mut self, r: &TeecapEvalResult) -> TeecapEvalResult {
        self.gen_r_a_b(TeecapOpRAB::Eq, r, &TeecapEvalResult::Const(0))
    }

    fn push_comment(&mut self, comment: &str) {
        self.push_asm_unit(TeecapAssemblyUnit::Comment(comment.to_string()));
    }

    fn push_insn(&mut self, insn: TeecapInsn) {
        self.push_asm_unit(TeecapAssemblyUnit::Insn(insn));
    }

    fn push_tag(&mut self, tag: TeecapNTag) {
        self.push_asm_unit(TeecapAssemblyUnit::Tag(tag));
    }
    
    fn push_passthrough_asm(&mut self, s: String) {
        self.push_asm_unit(TeecapAssemblyUnit::Passthrough(s));
    }

    fn gen_init_func_pre(&mut self) {
        self.push_asm_unit(TeecapAssemblyUnit::Passthrough("delin pc".to_string()));
        // set up the sc capability
        let stack_reg = TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None));
        let rheap = self.gen_splitlo_alloc_const(&stack_reg, CLI_ARGS.stack_size);

        let r = self.gen_splitlo_alloc_const(&stack_reg, TEECAP_SEALED_REGION_SIZE as u32);
        self.push_insn(TeecapInsn::Mov(TeecapReg::Sc, TEECAP_STACK_REG));
        self.push_insn(TeecapInsn::Mov(TEECAP_STACK_REG, r.reg));

        // setting the heap offset to 0
        self.gen_li(&r, TeecapImm::Const(0));
        self.push_insn(TeecapInsn::Scco(rheap.reg, r.reg));

        self.gen_store_with_cap(&TeecapOffset::Const(0), &stack_reg, &TeecapEvalResult::Register(rheap));
        self.release_gpr(&r);
    }

    fn gen_init_func_post(&mut self) {
        // jump to the main function
        let r = self.gen_li_alloc(TeecapImm::STag(TeecapSTag("_start".to_string()), true));
        self.push_insn(TeecapInsn::Jmp(r.reg));
        self.release_gpr(&r);
    }

    fn gen_mrev_alloc(&mut self, r: &TeecapRegResult) -> TeecapRegResult {
        let rrev = self.grab_new_gpr().expect("Failed to allocate GPR for mrev!");
        self.push_insn(TeecapInsn::Mrev(rrev.reg, r.reg));
        rrev
    }


    fn gen_splitlo_alloc(&mut self, rs: &TeecapRegResult, roffset: &TeecapRegResult) -> TeecapRegResult {
        let rsplit = self.grab_new_gpr().expect("Failed to allocate GPR for splitl");
        //let roffset = self.gen_li_alloc(TeecapImm::Const(offset as u64));
        self.push_insn(TeecapInsn::Splitlo(rsplit.reg, rs.reg, roffset.reg));
        //self.release_gpr(&roffset);
        rsplit
    }

    fn gen_splitlo_alloc_const(&mut self, rs: &TeecapRegResult, offset: u32) -> TeecapRegResult {
        let roffset = self.gen_li_alloc(TeecapImm::Const(offset as u64));
        let res = self.gen_splitlo_alloc(rs, &roffset);
        self.release_gpr(&roffset);
        res
    }

    fn gen_splitlo_alloc_const_reversible(&mut self, rs: &TeecapRegResult, offset: u32) -> (TeecapRegResult, TeecapRegResult) {
        let rrev = self.gen_mrev_alloc(rs);
        (self.gen_splitlo_alloc_const(rs, offset), rrev)
    }

    fn gen_sd_imm_offset(&mut self, rcap: &TeecapRegResult, rs: &TeecapRegResult, offset: &TeecapOffset) {
        let roffset = offset.to_register(self);
        self.push_insn(TeecapInsn::Scco(rcap.reg, roffset.reg));
        self.release_gpr(&roffset);
        self.push_insn(TeecapInsn::Sd(rcap.reg, rs.reg));
    }

    fn gen_seal_setup(&mut self, rseal: &TeecapRegResult, rpc: &TeecapRegResult, rstack: &TeecapRegResult) {
        self.gen_sd_imm_offset(rseal, rpc, &TeecapOffset::Const(TEECAP_SEALED_OFFSET_PC as u32));
        self.release_gpr(&rpc);
        //self.gen_sd_imm_offset(rseal, rstack, TEECAP_SEALED_OFFSET_STACK_REG as u32);
        // We instead pass the stack as the argument
        self.push_insn(TeecapInsn::Seal(rseal.reg));
    }

    fn gen_mov_alloc(&mut self, rs: &TeecapRegResult) -> TeecapRegResult {
        let rd = self.grab_new_gpr().expect("Failed to allocate GPR for mov!");
        self.push_insn(TeecapInsn::Mov(rd.reg, rs.reg));
        rd
    }

    fn gen_load_with_cap_alloc(&mut self, offset: &TeecapOffset, rcap: &TeecapRegResult) -> TeecapRegResult {
        let r = offset.to_register(self);
        self.push_insn(TeecapInsn::Scco(rcap.reg, r.reg));
        self.push_insn(TeecapInsn::Ld(r.reg, rcap.reg));
        r
    }

    fn gen_call_stack_setup(&mut self, rstack: &TeecapRegResult, args: &Vec<Node<Expression>>) {
        let mut offset = 0;
        for arg in args.iter() {
            let res = arg.teecap_evaluate(self);
            self.gen_store_with_cap(&TeecapOffset::Const(offset), rstack, &res);
            offset += res.get_size(self);
        }
    }

    // do not release sealed
    fn gen_call_on_reg(&mut self, sealed: &TeecapRegResult, args: &Vec<Node<Expression>>) -> TeecapEvalResult {
        let (rstack_callee, rrev) = self.gen_splitlo_alloc_const_reversible(
            &TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None)), self.reserved_stack_size);
        let rstack_rev = self.gen_mrev_alloc(&rstack_callee);
        self.gen_call_stack_setup(&rstack_callee, args);
        self.push_insn(TeecapInsn::Call(sealed.reg, rstack_callee.reg));
        self.release_gpr(&rstack_callee);

        // restore the stack
        self.push_insn(TeecapInsn::Lin(rstack_rev.reg));
        let res = self.gen_load_with_cap_alloc(&TeecapOffset::Const(0), &rstack_rev);
        self.push_insn(TeecapInsn::Drop(rstack_rev.reg)); // TODO: if the result is an uninitialised capability, initialise it first
        self.release_gpr(&rstack_rev);

        self.push_insn(TeecapInsn::Drop(TEECAP_STACK_REG));
        self.push_insn(TeecapInsn::Lin(rrev.reg));
        self.push_insn(TeecapInsn::Mov(TEECAP_STACK_REG, rrev.reg));
        self.release_gpr(&rrev);

        TeecapEvalResult::Register(res)
    }

    fn gen_call_in_group(&mut self, func_name: &str, args: &Vec<Node<Expression>>) -> TeecapEvalResult {
        let (rseal, rrev) = self.gen_splitlo_alloc_const_reversible(
            &TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None)), self.reserved_stack_size);
        let (rstack_callee, rrev2) = self.gen_splitlo_alloc_const_reversible(&rseal, TEECAP_SEALED_REGION_SIZE as u32);

        let rstack_rev = self.gen_mrev_alloc(&rstack_callee);
        // now rstack_callee contains a linear capability that points to a region of size
        // TEECAP_SEALED_REGION_SIZE
        let rpc = self.gen_mov_alloc(&TeecapRegResult::new_simple(TeecapReg::Pc, TeecapType::Cap(None)));
        let pc_addr_imm = TeecapImm::STag(TeecapSTag(func_name.to_string()), false);
        let rpc_addr = self.gen_li_alloc(pc_addr_imm);
        self.push_insn(TeecapInsn::Scc(rpc.reg, rpc_addr.reg));
        self.release_gpr(&rpc_addr);

        // set up the sealed capability
        self.gen_call_stack_setup(&rstack_callee, args);
        self.gen_seal_setup(&rseal, &rpc, &rstack_callee);
        self.push_insn(TeecapInsn::Call(rseal.reg, rstack_callee.reg));
        self.release_gpr(&rstack_callee);
        self.release_gpr(&rseal);

        // restore the stack/
        self.push_insn(TeecapInsn::Lin(rstack_rev.reg));
        let res = self.gen_load_with_cap_alloc(&TeecapOffset::Const(0), &rstack_rev);
        self.push_insn(TeecapInsn::Drop(rstack_rev.reg)); // TODO: if the result is an uninitialised capability, initialise it first
        self.release_gpr(&rstack_rev);

        //self.push_insn(TeecapInsn::Drop(rseal));

        self.push_insn(TeecapInsn::Lin(rrev2.reg));
        self.push_insn(TeecapInsn::Drop(rrev2.reg));
        self.release_gpr(&rrev2);

        self.push_insn(TeecapInsn::Drop(TEECAP_STACK_REG));
        self.push_insn(TeecapInsn::Lin(rrev.reg));
        self.push_insn(TeecapInsn::Mov(TEECAP_STACK_REG, rrev.reg));
        self.release_gpr(&rrev);

        TeecapEvalResult::Register(res)
    }

    fn in_cur_func_group(&self, func_name: &str) -> bool {
        true
        //self.get_cur_func_name() == func_name ||
        //self.get_cur_func_group_id() == self.function_map.get(func_name).expect("Undefined function").group
    }

    fn gen_retval_setup(&mut self, retval: &TeecapEvalResult) {
        match &self.cur_func.as_ref().unwrap().return_type {
            TeecapType::Void => {
                // We don't really need to do anything here
            }
            _ => {
                self.gen_store_with_cap(&TeecapOffset::Const(0), 
                    &TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None)), retval);
                self.push_insn(TeecapInsn::Drop(TEECAP_STACK_REG));
            }
        }
    }

    // ordinary return
    // return value should be passed through the stack
    fn gen_return(&mut self, retval: &TeecapEvalResult, sealed_repl: &TeecapEvalResult) {
        self.gen_retval_setup(retval);
        let r = sealed_repl.to_register(self);
        //self.push_insn(TeecapInsn::Drop(TeecapReg::Sc)); // FIXME: cannot directly drop sc; should not allow dropping sc
        // Save the context in the invoked sealed capability instead of sc
        // Temporary solution for now: allowing dropping of uninitialised caps
        self.push_insn(TeecapInsn::Drop(TEECAP_STACK_REG));
        self.release_gpr_ancestors(&r);
        self.push_insn(TeecapInsn::Ret(TeecapReg::Ret, r.reg));
        self.release_gpr_no_recurse(&r);
    }

    fn gen_return_sealed(&mut self, retval: &TeecapEvalResult, new_pc: &TeecapEvalResult) {
        self.gen_retval_setup(retval);
        let r = new_pc.to_register(self);
        //self.push_insn(TeecapInsn::Drop(TeecapReg::Sc));
        // do not drop sc because it should be sealed
        self.push_insn(TeecapInsn::Drop(TEECAP_STACK_REG));
        self.release_gpr_ancestors(&r);
        //self.push_insn(TeecapInsn::Out(TeecapReg::Pc));
        //self.push_insn(TeecapInsn::Out(r.reg));
        self.push_insn(TeecapInsn::RetSealed(TeecapReg::Ret, r.reg));
        self.release_gpr(&r);
    }

    fn gen_cap_query(&mut self, cap_var: &TeecapVariable,
                     insn_gen: fn(TeecapReg, TeecapReg) -> TeecapInsn) -> Option<TeecapEvalResult> {
        //eprintln!("{:?}", cap_var);
        let rcap = self.gen_ld_alloc(cap_var);
        //eprintln!("Reg: {:?}", rcap);
        let res = self.grab_new_gpr().expect("Failed to allocate GPR for cap query!");
        self.push_insn(insn_gen(res.reg, rcap.reg));
        //self.gen_store(cap_var, &TeecapEvalResult::Register(rcap.clone()));
        // we need to put it back in case it is a linear capability
        //eprintln!("rcap = {:?}, {:?}, {:?}", rcap, res, self.get_gpr_state(res.reg));
        self.release_gpr(&rcap);
        Some(TeecapEvalResult::Register(res))
    }
}


impl<T: TeecapEmitter> TeecapEmitter for Node<T> {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        self.node.teecap_emit_code(ctx);
    }
}

impl TeecapEvaluator for Constant {
   fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
       match &self {
           Constant::Integer(int) => {
               TeecapEvalResult::Const(int.to_teecap_literal())
           }
           _ => {
               TeecapEvalResult::Const(0)
           }
       }
   }
}

impl TeecapEvaluator for BinaryOperatorExpression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        let mut lhs = self.lhs.teecap_evaluate(ctx);
        let mut rhs = self.rhs.teecap_evaluate(ctx);
        match self.operator.node {
            BinaryOperator::Assign => {
                ctx.gen_assignment(&lhs, &rhs)
            }
            BinaryOperator::Plus => {
                ctx.gen_a_b(TeecapOpAB::Plus, &lhs, &rhs)
            }
            BinaryOperator::Minus => {
                ctx.gen_a_b(TeecapOpAB::Minus, &lhs, &rhs)
            }
            BinaryOperator::Multiply => {
                ctx.gen_a_b(TeecapOpAB::Mult, &lhs, &rhs)
            }
            BinaryOperator::Divide => {
                ctx.gen_a_b(TeecapOpAB::Div, &lhs, &rhs)
            }
            BinaryOperator::LogicalAnd => {
                ctx.gen_a_b(TeecapOpAB::And, &lhs, &rhs)
            }
            BinaryOperator::LogicalOr => {
                ctx.gen_a_b(TeecapOpAB::Or, &lhs, &rhs)
            }
            BinaryOperator::NotEquals => {
                let res = ctx.gen_r_a_b(TeecapOpRAB::Eq, &lhs, &rhs);
                ctx.gen_not(&res)
            }
            BinaryOperator::Equals => {
                ctx.gen_r_a_b(TeecapOpRAB::Eq, &lhs, &rhs)
            }
            BinaryOperator::LessOrEqual => {
                ctx.gen_r_a_b(TeecapOpRAB::Le, &lhs, &rhs)
            }
            BinaryOperator::Greater => {
                ctx.gen_r_a_b(TeecapOpRAB::Gt, &lhs, &rhs)
            }
            BinaryOperator::GreaterOrEqual => {
                ctx.gen_r_a_b(TeecapOpRAB::Ge, &lhs, &rhs)
            }
            BinaryOperator::Less => {
                ctx.gen_r_a_b(TeecapOpRAB::Lt, &lhs, &rhs)
            }
            BinaryOperator::Index => {
                ctx.gen_index(&mut lhs, &mut rhs)
            }
            _ => {
                TeecapEvalResult::Const(0)
            }
        }
    }
}

impl TeecapEvaluator for Identifier {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        TeecapEvalResult::UnresolvedVar(TeecapUnresolvedVar(self.name.to_string()))
    }
}

trait ToRegisters {
    fn to_registers(&self, ctx: &mut CodeGenContext) -> Vec<TeecapRegResult>;
}

impl ToRegisters for Vec<Node<Expression>> {
    fn to_registers(&self, ctx: &mut CodeGenContext) -> Vec<TeecapRegResult> {
        self.iter().map(|x| x.teecap_evaluate(ctx).to_register(ctx)).collect()
    }
}

trait TeecapTryIntoStr {
    fn teecap_try_into_str(&self) -> Option<&str>;
}

impl TeecapTryIntoStr for Expression {
    fn teecap_try_into_str(&self) -> Option<&str> {
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
impl TeecapEvaluator for CallExpression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        ctx.push_comment("Call expression");
        ctx.push_comment("Evaluating callee");
        let callee = self.callee.teecap_evaluate(ctx);
        ctx.push_comment("Evaluating arguments");
        ctx.push_comment("Arguments evaluated");
        let res = match &callee {
            TeecapEvalResult::UnresolvedVar(unresolved_var) => {
                match unresolved_var.0.as_str() {
                    // builtin pseudo calls
                    "print" => {
                        let args = self.arguments.to_registers(ctx);
                        let r = args.first()
                            .expect("Missing argument for print!");
                        ctx.push_insn(TeecapInsn::Out(r.reg));
                        ctx.release_gprs(&args);
                        //ctx.release_gpr_no_recurse(&r);
                        TeecapEvalResult::Const(0)
                    }
                    "exit" => {
                        ctx.push_insn(TeecapInsn::Halt);
                        TeecapEvalResult::Const(0)
                    }
                    "splitlo" => {
                        let args = self.arguments.to_registers(ctx);
                        let res = TeecapEvalResult::Register(ctx.gen_splitlo_alloc(&args[0], &args[1]));
                        ctx.release_gprs(&args);
                        res
                    }
                    "seal" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Seal(args.first().expect("Missing argument for seal!").reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "sealret" => {
                        let arg_rd = self.arguments.first().expect("Missing arguments for sealret!").teecap_evaluate(ctx).to_register(ctx);
                        let reg_name: &str = &*self.arguments[1].node.teecap_try_into_str().expect("Wrong argument type for reg!");
                        let arg_rs = TeecapReg::try_from_str(reg_name.trim_matches('\"')).expect("Unknown register name!");
                        ctx.push_insn(TeecapInsn::SealRet(arg_rd.reg, arg_rs));
                        ctx.release_gpr(&arg_rd);
                        TeecapEvalResult::Const(0)
                    }
                    "lin" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Lin(args.first().expect("Missing argument for lin!").reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "delin" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Delin(args.first().expect("Missing argument for delin!").reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "tighten" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Tighten(args[0].reg, args[1].reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "shrink" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Shrink(args[0].reg, args[1].reg, args[2].reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "scco" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Scco(args[0].reg, args[1].reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "scc" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Scc(args[0].reg, args[1].reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "mrev" => {
                        let args = self.arguments.to_registers(ctx);
                        let reg = ctx.gen_mrev_alloc(args.first().expect("Missing argument for mrev!"));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Register(reg)
                    }
                    "drop" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.push_insn(TeecapInsn::Drop(args.first().expect("Missing argument for drop!").reg));
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    "reg" => {
                        let s: &str = &*self.arguments.first().expect("Missing argument for reg!").node.teecap_try_into_str().expect("Wrong argument type for reg!");
                        let reg = TeecapReg::try_from_str(s.trim_matches('\"')).expect("Unknown register name!");
                        if !matches!(ctx.get_gpr_state(reg), TeecapRegState::Pinned) {
                            eprintln!("Warning: register {} is not pinned", reg);
                        }
                        TeecapEvalResult::Register(TeecapRegResult::new_simple(reg, TeecapType::Int))
                    }
                    "returnsl" => {
                        let args = self.arguments.to_registers(ctx);
                        ctx.gen_return_sealed(&TeecapEvalResult::Register(args[0].clone()), 
                            &TeecapEvalResult::Register(args[1].clone()));
                        TeecapEvalResult::Const(0)
                    }
                    "direct_call" => {
                        // does not do anything beyond a call instruction
                        let args = self.arguments.to_registers(ctx);
                        let dummy_arg = ctx.gen_li_alloc(TeecapImm::Const(0));
                        ctx.push_insn(TeecapInsn::Call(args[0].reg, dummy_arg.reg));
                        ctx.release_gpr(&dummy_arg);
                        ctx.release_gprs(&args);
                        TeecapEvalResult::Const(0)
                    }
                    _ => {
                        // TODO: support function call
                        // 1. create sealed capability region
                        // 2. create stack frame
                        // 3.jmove arguments into the stack frame
                        // 4. seal capability
                        // 5. create revocation capabilities
                        // 5. invoke sealed capability
                        // 6. linearise revocation capabilities
                        // drop capabilities before return
                        // 7. provides two kinds of function calls: cross domain and in-domain
                        // let's say for now that we want to allow everybody to be able to call
                        // every other function. In this case, we just use the same nonlinear RX
                        // capability
                        // FIXME: here we require that the called function be declared before
                        //let args_iter : Vec<TeecapEvalResult> = self.arguments.iter().map(|x| x.teecap_evaluate(ctx)).collect();
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
                                    TeecapEvalResult::Const(0)
                                }
                            }
                        }
                    }
                }
            }
            TeecapEvalResult::Variable(var) => {
                let reg = ctx.gen_ld_alloc(var);
                let res = ctx.gen_call_on_reg(&reg, &self.arguments);
                ctx.release_gpr(&reg);
                res
            }
            TeecapEvalResult::Register(reg) => {
                let res = ctx.gen_call_on_reg(&reg, &self.arguments);
                ctx.release_gpr(&reg);
                res
            }
            _ => {
                eprintln!("Function pointer not supported: {:?}", self.callee);
                TeecapEvalResult::Const(0)
            }
        };
        ctx.drop_result(&callee);
        res
    }
}

impl TeecapEvaluator for UnaryOperatorExpression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        let exp = self.operand.teecap_evaluate(ctx);
        match self.operator.node {
            UnaryOperator::Negate => {
                ctx.gen_not(&exp)
            }
            UnaryOperator::Indirection => {
                // TODO: *cap expression
                TeecapEvalResult::Const(0)
            }
            UnaryOperator::Address => {
                // TODO: &obj expresion
                TeecapEvalResult::Const(0) // support stack obj and in function call arguments only
            }
            _ => {
                exp
            }
        }
    }
}

impl TeecapEvaluator for MemberExpression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        let lhs = self.expression.teecap_evaluate(ctx);
        let rhs = self.identifier.teecap_evaluate(ctx);
        match self.operator.node {
            MemberOperator::Direct => {
                // a.b
                // for this type, directly compute the offset
                lhs.join_direct(&rhs, ctx).expect("Bad direct member expression!")
            }
            MemberOperator::Indirect => {
                //eprintln!("Indirect {:?} {:?}", lhs, rhs);
                //eprintln!("{:?}", ctx.variables);
                lhs.join_indirect(&rhs, ctx).expect("Bad indirect member expression!")
            }
        }
    }
}

impl TeecapEvaluator for Expression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        match &self {
            Expression::Constant(con) => {
                con.teecap_evaluate(ctx)
            }
            Expression::UnaryOperator(op) => {
                op.teecap_evaluate(ctx)
            }
            Expression::BinaryOperator(op) => {
                // TODO: we can enable some binary operations on either 
                // capabilities or the fields of capabilities
                // Example: + can be used to move cursor
                op.teecap_evaluate(ctx)
            }
            Expression::Identifier(id) => {
                id.teecap_evaluate(ctx)
            }
            Expression::Statement(stmt) => {
                stmt.teecap_emit_code(ctx);
                TeecapEvalResult::Const(0)
            }
            Expression::Call(call) => {
                call.teecap_evaluate(ctx)
            }
            Expression::Comma(exps) => {
                let mut last_res = TeecapEvalResult::Const(0);
                for e in exps.iter() {
                    ctx.drop_result(&last_res);
                    last_res = e.teecap_evaluate(ctx);
                }
                last_res
            }
            Expression::Member(member) => {
                member.teecap_evaluate(ctx)
            }
            Expression::SizeOfTy(st) => {
                let ttype = TeecapType::parse(ctx, &st.node.0.node).expect("Bad type specifier in sizeof!");
                TeecapEvalResult::Const(ttype.get_size(&ctx.struct_map) as u64)
            }
            _ => {
                TeecapEvalResult::Const(0)
            }
        }
    }
}

impl<T: TeecapEvaluator> TeecapEvaluator for Node<T> {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        self.node.teecap_evaluate(ctx)
    }
}



impl TeecapEmitter for InitDeclarator {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let name = get_decl_kind_name(&self.declarator.node.kind.node).expect("Bad variable name!");

        let init_val = match &self.initializer {
            Some(n) => {
                match &n.node {
                    Initializer::Expression(exp) => {
                        exp.teecap_evaluate(ctx)
                    }
                    _ => {
                        eprintln!("Literal format unsupported: {:?}", self.initializer);
                        TeecapEvalResult::Const(0)
                    }
                }
            }
            None => {
                TeecapEvalResult::Const(0)
            }
        };
        let var = ctx.resolve_top_var(&TeecapUnresolvedVar(name.to_string()))
            .expect("Failed to resolve var name for initialization!");
        ctx.gen_store_drop_result(&var, &init_val);
    }
}

#[derive(Debug)]
struct TeecapStruct {
    size: u32,
    name: String,
    members: TeecapScope,
}


type TeecapStructMap = HashMap<String, TeecapStruct>;

impl TeecapStruct {
    fn new(name: String, struct_map: &TeecapStructMap) -> TeecapStruct {
        let res = TeecapStruct {
            size: 0,
            name: name.clone(),
            members: TeecapScope::new()
        };
        res
    }

    fn add_field(&mut self, field: TeecapField, struct_map: &TeecapStructMap) {
        let field_size = field.get_size(struct_map);
        self.members.insert(field.name.clone(), (self.size, field));
        self.size += field_size;
    }

    fn get_field(&self, name: &str) -> Option<(u32, TeecapType)> {
        self.members.get(name).map(|(o, f)| (*o, f.field_type.clone()))
    }
}

// TODO: add a special type: capability
#[derive(Clone, Debug)]
enum TeecapType {
    Int,
    Void,
    Cap(Option<Box<TeecapType>>), // might be another capability associated with another type
    Struct(String),
    Array(Box<TeecapType>, u32)
}

impl TeecapType {
    fn get_size(&self, struct_map: &TeecapStructMap) -> u32 {
        match self {
            TeecapType::Int => 1,
            TeecapType::Void => 1,
            TeecapType::Cap(_) => 1,
            TeecapType::Struct(struct_name) => {
                struct_map.get(struct_name).expect("Struct undefined!").size
            },
            TeecapType::Array(base_type, size) => {
                base_type.get_size(struct_map) * size
            }
        }
    }

    fn is_cap(&self) -> bool {
        match self {
            TeecapType::Cap(_) => true,
            _ => false
        }
    }

    fn derived_decorate(&mut self, ctx: &mut CodeGenContext, derived: &DerivedDeclarator) {
        match &derived {
            DerivedDeclarator::Pointer(_) => {
                *self = TeecapType::Cap(Some(Box::new(self.clone())));
            }
            DerivedDeclarator::Array(array_decl) => {
                let size = match &array_decl.node.size {
                    ArraySize::VariableExpression(e) => {
                        let rsize = e.teecap_evaluate(ctx);
                        match rsize {
                            TeecapEvalResult::Const(size) => size,
                            _ => 1
                        }
                    }
                    _ => 1,
                };
                *self = TeecapType::Array(Box::new(self.clone()), size as u32);
            }
            _ => {
            }
        }
    }

    fn parse<T:AsDeclaration>(ctx: &mut CodeGenContext, declaration: &T) -> Option<TeecapType> {
        let mut t = declaration.get_specifiers().first()?.to_teecap_type(ctx);
        for decl in declaration.get_declarators().iter() {
            for d in decl.derived.iter() {
                t.derived_decorate(ctx, &d.node);
            }
        }
        Some(t)
    }
}


trait ToTeecapType {
    fn to_teecap_type(&self, ctx: &mut CodeGenContext) -> TeecapType;
}

#[derive(Debug)]
struct TeecapField {
    name: String,
    field_type: TeecapType,
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

impl TeecapField {
    fn parse_from_declarator(ctx: &mut CodeGenContext, decl: &Declarator, base_type: TeecapType) 
        -> TeecapField{
        let name = match &decl.kind.node {
            DeclaratorKind::Identifier(id) => {
                &id.node.name
            }
            _ => {
                panic!("No name specified for a field!");
            }
        };
        let mut ttype = base_type;
        //eprintln!("Decl = {:?}", decl.derived);
        for derived in decl.derived.iter() {
            ttype.derived_decorate(ctx, &derived.node);
        }
        //eprintln!("ttype = {:?}", ttype);
        TeecapField { name: name.to_string(), field_type: ttype }
    }

    fn parse<T:AsDeclaration> (ctx: &mut CodeGenContext, struct_field: &T) -> Vec<TeecapField>  {
        // only accept single specifier and declarator for now
        let field_type = struct_field.get_specifiers().first()
            .expect("No specifier for struct fields!").to_teecap_type(ctx);
        struct_field.get_declarators().iter().map(|x| {
            TeecapField::parse_from_declarator(ctx, &x, field_type.clone())
        }).collect()
    }
}

impl TeecapField {
    fn get_size(&self, struct_map: &TeecapStructMap) -> u32 {
        self.field_type.get_size(struct_map)
    }
}


impl ToTeecapType for StructType {
    fn to_teecap_type(&self, ctx: &mut CodeGenContext) -> TeecapType {
        if self.kind.node != StructKind::Struct {
            eprintln!("Union is not supported!");
        }
        let id = self.identifier.as_ref().expect("Anonymous struct not supported!");

        if let Some(decl) = &self.declarations {
            // if this comes with declarations
            let mut teecap_struct = TeecapStruct::new(id.node.name.clone(), &mut ctx.struct_map);
            //eprintln!("{:#?}", decl);
            for f in decl.iter() {
                match &f.node {
                    StructDeclaration::Field(field) => {
                        let fields = TeecapField::parse(ctx, &field.node);
                        for field in fields.into_iter() {
                            teecap_struct.add_field(field, &ctx.struct_map);
                        }
                    }
                    _ => {
                        eprintln!("Static assertion in struct not supported!");
                    }
                }
            }
            ctx.add_struct(teecap_struct);
        }

        TeecapType::Struct(id.node.name.clone())
    }
}

impl ToTeecapType for TypeSpecifier {
    fn to_teecap_type(&self, ctx: &mut CodeGenContext) -> TeecapType {
        match self {
            TypeSpecifier::Struct(struct_node) => {
                struct_node.to_teecap_type(ctx)
            }
            TypeSpecifier::Void => {
                TeecapType::Void
            }
            _ => {
                TeecapType::Int
            }
        }
    }
}

impl<T: ToTeecapType> ToTeecapType for Node<T> {
    fn to_teecap_type(&self, ctx: &mut CodeGenContext) -> TeecapType {
        self.node.to_teecap_type(ctx)
    }
}

impl TeecapEmitter for Declaration {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let fields = TeecapField::parse(ctx, self);
        for field in fields.into_iter() {
            ctx.add_var_to_scope(field);
        }
        for n in self.declarators.iter() {
            n.teecap_emit_code(ctx);
        }
    }
}

impl TeecapEmitter for BlockItem {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        match &self {
            BlockItem::Declaration(n) => {
                n.teecap_emit_code(ctx);
            }
            BlockItem::Statement(n) => {
                n.teecap_emit_code(ctx);
            }
            _ => {
                eprintln!("{:?} not processed", self);
            }
        }
    }
}

impl TeecapEmitter for IfStatement {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let tag_else = ctx.alloc_tag();
        let reg_cond = self.condition.teecap_evaluate(ctx).to_register(ctx);

        let reg_else = ctx.gen_li_alloc(TeecapImm::NTag(tag_else, true));
        ctx.push_insn(TeecapInsn::Jz(reg_else.reg, reg_cond.reg));
        ctx.release_gpr(&reg_cond);
        ctx.release_gpr(&reg_else);
        
        // then clause
        ctx.push_scope();
        self.then_statement.teecap_emit_code(ctx);
        ctx.pop_scope();

        ctx.push_tag(tag_else);

        if let Some(else_stmt) = &self.else_statement {
            // continuation of then clause: skip the else clause
            let tag_end = ctx.alloc_tag();
            let reg_end = ctx.gen_li_alloc(TeecapImm::NTag(tag_end, true));
            ctx.push_insn(TeecapInsn::Jmp(reg_end.reg));
            ctx.release_gpr(&reg_end);

            // else clause
            ctx.push_tag(tag_else);

            ctx.push_scope();
            else_stmt.teecap_emit_code(ctx);
            ctx.pop_scope();
            
            // end
            ctx.push_tag(tag_end);
        }
    }
}

impl TeecapEmitter for WhileStatement {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let tag_start = ctx.alloc_tag();
        let tag_end = ctx.alloc_tag();

        ctx.push_tag(tag_start);
        let reg_cond = self.expression.teecap_evaluate(ctx).to_register(ctx);
        let reg_end = ctx.gen_li_alloc(TeecapImm::NTag(tag_end, true));
        ctx.push_insn(TeecapInsn::Jz(reg_end.reg, reg_cond.reg));
        ctx.release_gpr(&reg_end);
        ctx.release_gpr(&reg_cond);

        // while body
        ctx.push_scope();
        ctx.push_break_cont(tag_end, tag_start);
        self.statement.teecap_emit_code(ctx);
        ctx.pop_break_cont();
        ctx.pop_scope();
        
        // end
        ctx.gen_jmp_tag(tag_start);
        ctx.push_tag(tag_end);
    }
}

// TODO: add do-while statement

impl TeecapEmitter for AsmStatement {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
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

impl TeecapEmitter for Statement {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        match &self {
            Statement::Compound(v) => {
                for n in v.iter() {
                    n.teecap_emit_code(ctx);
                }
            }
            Statement::Expression(exp) => {
                if let Some(e) = exp {
                    let res = e.teecap_evaluate(ctx);
                    ctx.drop_result(&res);
                }
            }
            Statement::If(if_stmt) => {
                if_stmt.teecap_emit_code(ctx);
            }
            Statement::While(while_stmt) => {
                while_stmt.teecap_emit_code(ctx);
            }
            Statement::Break => {
                ctx.gen_break();
            }
            Statement::Continue => {
                ctx.gen_continue();
            }
            Statement::Asm(asm_stmt) => {
                asm_stmt.teecap_emit_code(ctx); 
            }
            Statement::Return(ret) => {
                let retval =
                    if let Some(exp) = ret {
                        exp.teecap_evaluate(ctx)
                    } else{
                        TeecapEvalResult::Const(0)
                    };
                ctx.gen_return(&retval, &TeecapEvalResult::Const(0));
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

impl TeecapEmitter for ParameterDeclaration {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let fields = TeecapField::parse(ctx, self);
        for field in fields.into_iter() {
            ctx.add_var_to_scope(field);
        }
    }
}

impl TeecapEmitter for FunctionDeclarator {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        for param in self.parameters.iter() {
            param.teecap_emit_code(ctx);
        }
    }
}

impl TeecapEmitter for DerivedDeclarator {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        match self {
            DerivedDeclarator::Function(func_declarator) => {
                func_declarator.teecap_emit_code(ctx);
            }
            _ => {}
        }
    }
}

impl TeecapEmitter for Declarator {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        for derived in self.derived.iter() {
            derived.teecap_emit_code(ctx);
        }
    }
}

impl TeecapEmitter for FunctionDefinition {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let func_name = get_decl_kind_name(&self.declarator.node.kind.node)
            .expect("Bad function name!");
        let func_type = TeecapFunctionType::from_specifiers(&self.specifiers);
        let func_return_type = TeecapType::parse(ctx, self).expect("Function return type missing!");
        // emit code for body
        ctx.enter_function(func_name, func_type, func_return_type);
        ctx.push_scope();
        self.declarator.teecap_emit_code(ctx);
        self.statement.teecap_emit_code(ctx);
        ctx.pop_scope();
        ctx.exit_function();
    }
}

impl TeecapEmitter for ExternalDeclaration {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        match self {
            ExternalDeclaration::Declaration(n) => {
                n.teecap_emit_code(ctx);
            }
            ExternalDeclaration::StaticAssert(_n) => {
            }
            ExternalDeclaration::FunctionDefinition(n) => {
                n.teecap_emit_code(ctx);
            }
        }
    }
}

fn generate_code(trans_unit: &TranslationUnit) {
    let mut ctx = CodeGenContext::new();
    ctx.gen_init_func_pre();
    for node in trans_unit.0.iter() {
        node.teecap_emit_code(&mut ctx);
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

