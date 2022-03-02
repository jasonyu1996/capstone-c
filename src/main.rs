#![feature(box_patterns)]

use std::any::type_name;
use std::env::var_os;
use std::fmt::Display;
use std::os::unix::process::parent_id;
/**
 * Features omitted: no distinguishing different int sizes
 *
 * */


use std::{iter::Map, collections::HashMap};
use std::str::FromStr;

use lang_c::{driver::{Config, parse}, ast::{TranslationUnit, FunctionDefinition,
    StaticAssert, Declarator, Declaration, DeclaratorKind, Statement, BlockItem, InitDeclarator, Integer, Initializer, Expression, Constant}};
use lang_c::ast::{ExternalDeclaration, BinaryOperatorExpression, BinaryOperator, Identifier, IfStatement, WhileStatement, CallExpression, AsmStatement, UnaryOperatorExpression, UnaryOperator, MemberExpression, MemberOperator, DeclarationSpecifier, TypeSpecifier, StructType, StructKind, StructDeclaration, StructField, SpecifierQualifier, DerivedDeclarator, FunctionDeclarator, ParameterDeclaration, StructDeclarator};
use lang_c::span::Node;

const TEECAP_GPR_N: usize = 32; // number of general-purpose registers
const TEECAP_STACK_REG: TeecapReg = TeecapReg::Gpr(0); // r0 is used for storing the stack capability
const TEECAP_DEFAULT_MEM_SIZE: u32 = 1<<16;
const TEECAP_SEALED_REGION_SIZE: usize = TEECAP_GPR_N + 4;

const TEECAP_SEALED_OFFSET_PC: usize = 0;
const TEECAP_SEALED_OFFSET_STACK_REG: usize = 4;
const TEECAP_STACK_SIZE: u32 = 1<<14;

type TeecapInt = u64;


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
    NTag(TeecapNTag),
    STag(TeecapSTag),
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
    Add(TeecapReg, TeecapReg),
    Sub(TeecapReg, TeecapReg),
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
    Seal(TeecapReg),
    Drop(TeecapReg),
    Ret(TeecapReg, TeecapReg),
    RetSealed(TeecapReg, TeecapReg),
    Jmp(TeecapReg),
    Out(TeecapReg),
    Halt
}

impl Display for TeecapImm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapImm::NTag(tag) => {
                tag.fmt(f)
            }
            TeecapImm::STag(tag) => {
                tag.fmt(f)
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

    fn new_writeback(reg: TeecapReg, wb_to: TeecapRegResult, dtype: TeecapType) -> TeecapRegResult {
        TeecapRegResult { reg: reg, release_strategy: TeecapRegRelease::WriteBack(Box::new(wb_to)), data_type: dtype }
    }

    fn set_writeback(&mut self, wb_to: TeecapRegResult) {
        self.release_strategy = TeecapRegRelease::WriteBack(Box::new(wb_to));
    }
}

#[derive(Clone, Debug)]
enum TeecapRegRelease {
    Simple,
    WriteBack(Box<TeecapRegResult>)
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
            TeecapInsn::Mrev(rd, rs) => {
                write!(f, "mrev {} {}", rd, rs)
            }
            TeecapInsn::Mov(rd, rs) => {
                write!(f, "mov {} {}", rd, rs)
            }
            TeecapInsn::Seal(r) => {
                write!(f, "seal {}", r)
            }
            TeecapInsn::Call(r, ra) => {
                write!(f, "call {} {}", r, ra)
            }
            TeecapInsn::Lin(r) => {
                write!(f, "lin {}", r)
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
    Passthrough(String)
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
        }
    }
}

type TeecapFunctionGroupId = u32;


struct TeecapFunction {
    group: TeecapFunctionGroupId,
    name: String, // name of the function
    code: Vec<TeecapAssemblyUnit>
}

impl TeecapFunction {
    fn new(name: &str) -> TeecapFunction {
        TeecapFunction {
            group: 0,
            name: name.to_string(),
            code: Vec::new()
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
    offset_in_cap: u32, // offset inside the capability
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
        match &self.ttype {
            TeecapType::Cap(base_type) => { // indirect join can only be performed on a capability
                match base_type {
                    Some(box TeecapType::Struct(base_struct_name)) => {
                        let reg_lhs = ctx.gen_ld_alloc(self); // holds a capability
                        //ctx.push_insn(TeecapInsn::Out(reg_lhs.reg));
                        let (offset, ttype) = ctx.resolve_var(0, &Some(base_struct_name.to_string()), other)?;
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
                        let cap_end = ctx.gen_cap_query(self, TeecapInsn::Lce)?;
                        let cap_base = ctx.gen_cap_query(self, TeecapInsn::Lcb)?;
                        ctx.gen_a_b(TeecapOpAB::Minus, &cap_end, &cap_base);
                        ctx.drop_result(&cap_base);
                        Some(cap_end)
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
    fn to_register(&self, ctx: &mut CodeGenContext) -> TeecapRegResult {
        self.to_register_with_offset(0, ctx).unwrap() // should not fail at offset 0
    }

    fn to_register_with_offset(&self, offset: u32, ctx: &mut CodeGenContext) -> Option<TeecapRegResult> {
        if self.get_size(ctx) <= offset {
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
                Some(ctx.gen_ld_alloc_extra_offset(var, offset)) // note: here the type and size 
                    // information will be lost
            }
            TeecapEvalResult::UnresolvedVar(unresolved_var) => {
                ctx.resolve_top_var(unresolved_var).map(|x| ctx.gen_ld_alloc_extra_offset(&x, offset))
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
    fn get_size(&self, ctx: &CodeGenContext) -> u32 {
        match self {
            TeecapEvalResult::Const(_) => 1,
            TeecapEvalResult::Register(_) => 1,
            TeecapEvalResult::Variable(var) => {
                //eprintln!("{:#?}", (offset, parent, name, ctx.variables.first().unwrap(), &ctx.struct_map));
                var.ttype.get_size(&ctx.struct_map)
            }
            TeecapEvalResult::UnresolvedVar(var) => {
                ctx.resolve_top_var(var).map(|x| x.ttype.get_size(&ctx.struct_map)).unwrap_or(0)
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

type TeecapFunctionMap = HashMap<String, TeecapFunction>;

struct CodeGenContext {
    tag_count: u32,
    init_func: TeecapFunction,
    cur_func: Option<TeecapFunction>,
    function_map: TeecapFunctionMap,
    variables: Vec<TeecapScope>,
    reserved_stack_size: u32,
    gprs: Vec<bool>,
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
    Or
}


impl CodeGenContext {
    fn new() -> CodeGenContext {
        let mut instance = CodeGenContext {
            tag_count : 0,
            function_map: TeecapFunctionMap::new(),
            cur_func: None,
            variables: vec![TeecapScope::new()],
            reserved_stack_size: 0,
            gprs: vec![false; TEECAP_GPR_N],
            in_func: false,
            init_func: TeecapFunction::new("init"),
            break_conts: Vec::new(),
            struct_map: TeecapStructMap::new()
        };
        if let TeecapReg::Gpr(n) = TEECAP_STACK_REG {
            instance.gprs[n as usize] = true; // reserve the gpr for the stack cap
        }
        instance
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

    fn enter_function(&mut self, func_name: &str) {
        self.clear_gprs();
        self.in_func = true;
        self.reserved_stack_size = 0;
        self.cur_func = Some(TeecapFunction::new(func_name));

        let r = self.gen_li_alloc(TeecapImm::Const(0));
        self.push_insn(TeecapInsn::Scco(TEECAP_STACK_REG, r.reg));
        self.release_gpr(&r);
    }

    fn exit_function(&mut self) {
        self.gen_return(&TeecapEvalResult::Const(0));
        self.clear_gprs();

        let cur_func = std::mem::take(&mut self.cur_func).expect("Error exit_function: current function not found!");
        self.function_map.insert(cur_func.name.clone(), cur_func);
        self.in_func = false;
    }

    fn grab_gpr(&mut self, reg: TeecapReg) {
        if let TeecapReg::Gpr(n) = reg {
            self.gprs[n as usize] = true;
        }
    }

    fn grab_new_gpr(&mut self) -> Option<TeecapRegResult> {
        let some_n = self.gprs.iter().enumerate().find(|&(idx, &b)| !b).map(|x| x.0 as u8);
        if let &Some(n) = &some_n {
            self.gprs[n as usize] = true;
        }
        some_n.map(|x| TeecapRegResult::new_simple(TeecapReg::Gpr(x), TeecapType::Int))
    }

    fn release_gpr(&mut self, reg: &TeecapRegResult) {
        match &reg.release_strategy {
            TeecapRegRelease::Simple => {
                // no need to do anything
            }
            TeecapRegRelease::WriteBack(wb_to) => {
                self.push_insn(TeecapInsn::Sd(wb_to.reg, reg.reg));
                self.release_gpr(wb_to);
            }
        }
        if reg.reg != TEECAP_STACK_REG {
            if let &TeecapReg::Gpr(n) = &reg.reg {
                self.gprs[n as usize] = false;
            }
        }
    }

    fn release_gpr_no_recurse(&mut self, reg: &TeecapRegResult) {
        if reg.reg != TEECAP_STACK_REG {
            if let &TeecapReg::Gpr(n) = &reg.reg {
                self.gprs[n as usize] = false;
            }
        }
    }

    fn clear_gprs(&mut self) {
        self.gprs.iter_mut().for_each(|x| *x = false);
        self.grab_gpr(TEECAP_STACK_REG);
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

    fn gen_ld_extra_offset(&mut self, reg: &mut TeecapRegResult, var: &TeecapVariable, offset: u32) {
        let cap_reg = self.gen_load_cap_extra_offset(var, offset).expect("Failed to obtain capability for ld!");
        self.push_insn(TeecapInsn::Ld(reg.reg, cap_reg.reg));
        if var.ttype.is_cap() {
            reg.set_writeback(cap_reg);
        } else{
            self.release_gpr(&cap_reg); // TODO: we should not reuse cap registers like this
        }
        // it is necessary to write them back if the capability is linear
    }

    fn gen_ld_alloc_extra_offset(&mut self, var: &TeecapVariable, offset: u32) -> TeecapRegResult {
        let mut reg = self.grab_new_gpr().expect("Gpr allocation for ld failed!");
        self.gen_ld_extra_offset(&mut reg, var, offset);
        reg
    }

    fn gen_ld_alloc(&mut self, var: &TeecapVariable) -> TeecapRegResult {
        self.gen_ld_alloc_extra_offset(var, 0)
    }

    // the var is stored on stack
    fn resolve_top_var(&self, var: &TeecapUnresolvedVar) -> Option<TeecapVariable> {
        let (offset, ttype) = self.resolve_var(0, &None, var)?;
        Some(TeecapVariable {
            offset_in_cap: offset,
            ttype: ttype,
            cap: TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None))
        })
    }

    fn resolve_var(&self, offset: u32, parent_struct: &Option<String>, var: &TeecapUnresolvedVar) -> Option<(u32, TeecapType)> {
        match parent_struct {
            Some(parent_name) => {
                let par = self.struct_map.get(parent_name)?;
                //eprintln!("{:#?}", par);
                par.get_field(&var.0).map(|(o, t)| (offset + o, t))
            }
            None => {
                self.find_var_in_scope(&var.0).map(|(o, t)| (offset + o, t))
            }
        }
    }

    fn gen_load_cap_extra_offset(&mut self, var: &TeecapVariable, extra_offset: u32) -> Option<TeecapRegResult> {
        let offset_reg = self.gen_li_alloc(TeecapImm::Const((var.offset_in_cap + extra_offset) as u64));
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
        self.gen_load_cap_extra_offset(var, 0)
    }

    fn gen_store_with_cap(&mut self, offset: u32, rcap: &TeecapRegResult, val: &TeecapEvalResult) {
        let rvalue_size = val.get_size(self);
        for i in 0..rvalue_size {
            let reg = val.to_register_with_offset(i, self).unwrap();
            self.gen_sd_imm_offset(rcap, &reg, offset + i);
            self.release_gpr(&reg);
        }
    }

    fn gen_store(&mut self, var: &TeecapVariable, val: &TeecapEvalResult) -> TeecapEvalResult {
        let rvalue_size = val.get_size(self);
        for i in 0..rvalue_size {
            let reg = val.to_register_with_offset(i, self).unwrap();
            let cap_reg = self.gen_load_cap_extra_offset(var, i).expect("Unable to access variable through capabilities!");
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
                self.gen_store(&self.resolve_top_var(var).expect("Failed to resolve variable for assignment!"), rhs)
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
            TeecapOpAB::Or => {
                TeecapInsn::Or(rd.reg, rs.reg)
            }
        });
        self.release_gpr(&rs);
        TeecapEvalResult::Register(rd)
    }

    fn print_code(&self) {
        println!("{} {} {} {} {}", TEECAP_DEFAULT_MEM_SIZE, TEECAP_GPR_N, 0, 1, -1);
        println!("{} {} {}", 0, ":<stack>", ":<init>"); // the first is the pc
        println!("{} {} {}", ":<stack>", TEECAP_DEFAULT_MEM_SIZE, ":<stack>");
        let mut mem_offset = 0;
        self.init_func.print_code(&mut mem_offset);
        for func in self.function_map.values() {
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
        let target_reg = self.gen_li_alloc(TeecapImm::NTag(tag));
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
        let rheap = self.gen_splitlo_alloc(&stack_reg, TEECAP_STACK_SIZE);

        let r = self.gen_splitlo_alloc(&stack_reg, TEECAP_SEALED_REGION_SIZE as u32);
        self.push_insn(TeecapInsn::Mov(TeecapReg::Sc, TEECAP_STACK_REG));
        self.push_insn(TeecapInsn::Mov(TEECAP_STACK_REG, r.reg));

        // setting the heap offset to 0
        self.gen_li(&r, TeecapImm::Const(0));
        self.push_insn(TeecapInsn::Scco(rheap.reg, r.reg));

        self.gen_store_with_cap(0, &stack_reg, &TeecapEvalResult::Register(rheap));
        self.release_gpr(&r);
    }

    fn gen_init_func_post(&mut self) {
        // jump to the main function
        let r = self.gen_li_alloc(TeecapImm::STag(TeecapSTag("main".to_string())));
        self.push_insn(TeecapInsn::Jmp(r.reg));
        self.release_gpr(&r);
    }

    fn gen_mrev_alloc(&mut self, r: &TeecapRegResult) -> TeecapRegResult {
        let rrev = self.grab_new_gpr().expect("Failed to allocate GPR for mrev!");
        self.push_insn(TeecapInsn::Mrev(rrev.reg, r.reg));
        rrev
    }


    fn gen_splitlo_alloc(&mut self, rs: &TeecapRegResult, offset: u32) -> TeecapRegResult {
        let rsplit = self.grab_new_gpr().expect("Failed to allocate GPR for splitl");
        let roffset = self.gen_li_alloc(TeecapImm::Const(offset as u64));
        self.push_insn(TeecapInsn::Splitlo(rsplit.reg, rs.reg, roffset.reg));
        self.release_gpr(&roffset);
        rsplit
    }

    fn gen_splitlo_alloc_reversible(&mut self, rs: &TeecapRegResult, offset: u32) -> (TeecapRegResult, TeecapRegResult) {
        let rrev = self.gen_mrev_alloc(rs);
        (self.gen_splitlo_alloc(rs, offset), rrev)
    }

    fn gen_sd_imm_offset(&mut self, rcap: &TeecapRegResult, rs: &TeecapRegResult, offset: u32) {
        let roffset = self.gen_li_alloc(TeecapImm::Const(offset as u64));
        self.push_insn(TeecapInsn::Scco(rcap.reg, roffset.reg));
        self.release_gpr(&roffset);
        self.push_insn(TeecapInsn::Sd(rcap.reg, rs.reg));
    }

    fn gen_seal_setup(&mut self, rseal: &TeecapRegResult, rpc: &TeecapRegResult, rstack: &TeecapRegResult) {
        self.gen_sd_imm_offset(rseal, rpc, TEECAP_SEALED_OFFSET_PC as u32);
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

    fn gen_call_stack_setup(&mut self, rstack: &TeecapRegResult, args: &Vec<Node<Expression>>) {
        let mut offset = 0;
        for arg in args.iter() {
            let res = arg.teecap_evaluate(self);
            self.gen_store_with_cap(offset, rstack, &res);
            offset += res.get_size(self);
        }
    }

    fn gen_call_in_group(&mut self, func_name: &str, args: &Vec<Node<Expression>>) -> TeecapEvalResult {
        let (rseal, rrev) = self.gen_splitlo_alloc_reversible(
            &TeecapRegResult::new_simple(TEECAP_STACK_REG, TeecapType::Cap(None)), self.reserved_stack_size);
        let (rstack_callee, rrev2) = self.gen_splitlo_alloc_reversible(&rseal, TEECAP_SEALED_REGION_SIZE as u32);

        let rstack_rev = self.gen_mrev_alloc(&rstack_callee);
        // now rstack_callee contains a linear capability that points to a region of size
        // TEECAP_SEALED_REGION_SIZE
        let rpc = self.gen_mov_alloc(&TeecapRegResult::new_simple(TeecapReg::Pc, TeecapType::Cap(None)));
        let pc_addr_imm = TeecapImm::STag(TeecapSTag(func_name.to_string()));
        let rpc_addr = self.gen_li_alloc(pc_addr_imm);
        self.push_insn(TeecapInsn::Scc(rpc.reg, rpc_addr.reg));
        self.release_gpr(&rpc_addr);

        // set up the sealed capability
        self.gen_call_stack_setup(&rstack_callee, args);
        self.gen_seal_setup(&rseal, &rpc, &rstack_callee);
        self.push_insn(TeecapInsn::Call(rseal.reg, rstack_callee.reg));
        self.release_gpr(&rstack_callee);
        //self.release_gpr(&rseal);

        // restore the stack/
        self.push_insn(TeecapInsn::Lin(rstack_rev.reg));
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

        TeecapEvalResult::Register(rseal)
    }

    fn in_cur_func_group(&self, func_name: &str) -> bool {
        self.get_cur_func_name() == func_name ||
        self.get_cur_func_group_id() == self.function_map.get(func_name).expect("Undefined function").group
    }

    fn gen_return(&mut self, retval: &TeecapEvalResult) {
        let retreg = retval.to_register(self);
        self.push_insn(TeecapInsn::Drop(TeecapReg::Sc));
        self.push_insn(TeecapInsn::Drop(TEECAP_STACK_REG));
        self.push_insn(TeecapInsn::Ret(TeecapReg::Ret, retreg.reg));
        self.release_gpr(&retreg);
    }

    fn gen_cap_query(&mut self, cap_var: &TeecapVariable,
                     insn_gen: fn(TeecapReg, TeecapReg) -> TeecapInsn) -> Option<TeecapEvalResult> {
        let rcap = self.gen_ld_alloc(cap_var);
        let res = self.grab_new_gpr().expect("Failed to allocate GPR for cap query!");
        self.push_insn(insn_gen(res.reg, rcap.reg));
        //self.gen_store(cap_var, &TeecapEvalResult::Register(rcap.clone()));
        // we need to put it back in case it is a linear capability
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
        let lhs = self.lhs.teecap_evaluate(ctx);
        let rhs = self.rhs.teecap_evaluate(ctx);
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
        let callee = self.callee.teecap_evaluate(ctx);
        let args: Vec<TeecapRegResult> = self.arguments.iter().map(|x| x.teecap_evaluate(ctx).to_register(ctx)).collect();
        let res = match &callee {
            TeecapEvalResult::UnresolvedVar(unresolved_var) => {
                match unresolved_var.0.as_str() {
                    "print" => {
                        let r = args.first()
                            .expect("Missing argument for print!");
                        ctx.push_insn(TeecapInsn::Out(r.reg));
                        ctx.release_gpr_no_recurse(&r);
                        TeecapEvalResult::Const(0)
                    }
                    "exit" => {
                        ctx.push_insn(TeecapInsn::Halt);
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
                        if ctx.in_cur_func_group(&unresolved_var.0){
                            ctx.gen_call_in_group(&unresolved_var.0, &self.arguments)
                        } else{
                            eprintln!("Cross-group function call not supported yet: {}", &unresolved_var.0);
                            TeecapEvalResult::Const(0)
                        }
                    }
                }
            }
            _ => {
                eprintln!("Function pointer not supported: {:?}", self.callee);
                TeecapEvalResult::Const(0)
            }
        };
        ctx.drop_result(&callee);
        for arg in args.iter() {
            ctx.release_gpr(&arg);
        }
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
                lhs.join_direct(&rhs, ctx).expect("Bad member expression!")
            }
            MemberOperator::Indirect => {
                lhs.join_indirect(&rhs, ctx).expect("Bad member expression!")
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
        ctx.gen_store_drop_result(&ctx.resolve_top_var(&TeecapUnresolvedVar(name.to_string()))
            .expect("Failed to resolve var name for initialization!"), &init_val);
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
    Cap(Option<Box<TeecapType>>), // might be another capability associated with another type
    Struct(String)
}

impl TeecapType {
    fn get_size(&self, struct_map: &TeecapStructMap) -> u32 {
        match self {
            TeecapType::Int => 1,
            TeecapType::Cap(_) => 1,
            TeecapType::Struct(struct_name) => {
                struct_map.get(struct_name).expect("Struct undefined!").size
            }
        }
    }

    fn is_cap(&self) -> bool {
        match self {
            TeecapType::Cap(_) => true,
            _ => false
        }
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
            match derived.node {
                DerivedDeclarator::Pointer(_) => {
                    ttype = TeecapType::Cap(Some(Box::new(ttype)))
                }
                _ => {
                }
            }
        }
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

        let reg_else = ctx.gen_li_alloc(TeecapImm::NTag(tag_else));
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
            let reg_end = ctx.gen_li_alloc(TeecapImm::NTag(tag_end));
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
        let reg_end = ctx.gen_li_alloc(TeecapImm::NTag(tag_end));
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
                ctx.gen_return(&retval);
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
        // emit code for body
        ctx.enter_function(func_name);
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
            ExternalDeclaration::StaticAssert(n) => {
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
    match parse(&config, std::env::args().nth(1).expect("Missing source file name!")) {
        Ok(parser_result)  => {
            if std::env::var("TEECAP_SHOW_AST").is_ok() {
                println!("AST:\n{:#?}", &parser_result);
            } 
            generate_code(&parser_result.unit);
        }
        Err(e) => {
            eprintln!("Parse error!");
            eprintln!("{:?}", e);
        }
    }
}

