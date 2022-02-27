use std::any::type_name;
use std::fmt::Display;
/**
 * Features omitted: no distinguishing different int sizes
 *
 * */


use std::{iter::Map, collections::HashMap};
use std::str::FromStr;

use lang_c::{driver::{Config, parse}, ast::{TranslationUnit, FunctionDefinition,
    StaticAssert, Declarator, Declaration, DeclaratorKind, Statement, BlockItem, InitDeclarator, Integer, Initializer, Expression, Constant}};
use lang_c::ast::{ExternalDeclaration, BinaryOperatorExpression, BinaryOperator, Identifier, IfStatement, WhileStatement, CallExpression, AsmStatement, UnaryOperatorExpression, UnaryOperator, MemberExpression, MemberOperator, DeclarationSpecifier, TypeSpecifier, StructType, StructKind, StructDeclaration, StructField, SpecifierQualifier};
use lang_c::span::Node;

const TEECAP_GPR_N: usize = 32; // number of general-purpose registers
const TEECAP_STACK_REG: TeecapReg = TeecapReg::Gpr(0); // r0 is used for storing the stack capability
const TEECAP_DEFAULT_MEM_SIZE: u32 = 1<<16;

type TeecapInt = u64;


#[derive(Clone, Copy, PartialEq, Debug)]
enum TeecapReg {
    Pc, Id, Epc, Ret, Gpr(u8)
}

impl Display for TeecapReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
           TeecapReg::Pc => {
               write!(f, "pc")
           }
           TeecapReg::Id => {
               write!(f, "id")
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

#[derive(Clone, Copy, Debug)]
enum TeecapImm {
    Tag(TeecapTag),
    Const(TeecapInt)
}

#[derive(Clone, Copy)]
enum TeecapInsn {
    Li(TeecapReg, TeecapImm),
    Sd(TeecapReg, TeecapReg),
    Ld(TeecapReg, TeecapReg),
    Scc(TeecapReg, TeecapReg),
    Scco(TeecapReg, TeecapReg),
    Add(TeecapReg, TeecapReg),
    Sub(TeecapReg, TeecapReg),
    Jnz(TeecapReg, TeecapReg),
    Jz(TeecapReg, TeecapReg),
    Eq(TeecapReg, TeecapReg, TeecapReg),
    Le(TeecapReg, TeecapReg, TeecapReg),
    Lt(TeecapReg, TeecapReg, TeecapReg),
    And(TeecapReg, TeecapReg),
    Or(TeecapReg, TeecapReg),
    Jmp(TeecapReg),
    Out(TeecapReg),
    Halt
}

impl Display for TeecapImm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapImm::Tag(tag) => {
                tag.fmt(f)
            }
            TeecapImm::Const(c) => {
                c.fmt(f)
            }
        }
    }
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
            _ => {
                write!(f, "<und>")
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TeecapTag(u32);

enum TeecapAssemblyUnit {
    Tag(TeecapTag),
    Insn(TeecapInsn),
    Passthrough(String)
}

impl Display for TeecapTag {
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


struct TeecapFunction {
    name: String, // name of the function
    code: Vec<TeecapAssemblyUnit>
}

impl TeecapFunction {
    fn new(name: &str) -> TeecapFunction {
        TeecapFunction {
            name: name.to_string(),
            code: Vec::new()
        }
    }

    fn push_asm_unit(&mut self, asm_unit: TeecapAssemblyUnit) {
        self.code.push(asm_unit);
    }

    fn print_code(&self, mem_offset: &mut u32) {
        // TODO: add the segment definition as well
        println!("{} {}", mem_offset, -1);
        println!(":<{}>", self.name);
        for asm_unit in self.code.iter() {
            asm_unit.print_code(mem_offset);
        }
        println!("$");
    }
}

//#[derive(Debug)]
//struct TeecapVariable {
    //name: String,
    //offset: u32 // offset of the variable in the stack frame
    ////init_value: TeecapInt // only supporting unsigned 64-bit
//}

type TeecapScope = HashMap<String, (u32, TeecapField)>;

#[derive(Debug)]
enum TeecapEvalResult {
    Const(TeecapInt),
    Register(TeecapReg),
    Variable(u32, Option<String>, String) // parent offset, parent type, name
}

impl TeecapEvalResult {
    fn to_register(&self, ctx: &mut CodeGenContext) -> TeecapReg {
        match self {
            &TeecapEvalResult::Const(n) => {
                ctx.gen_li_alloc(TeecapImm::Const(n))
            }
            TeecapEvalResult::Register(r) => {
                r.clone()
            }
            TeecapEvalResult::Variable(offset, parent_struct, r_var_name) => {
                // load the variable
                ctx.gen_ld_alloc(*offset, parent_struct, r_var_name)
            }
        }
    }
    fn join(&self, other: &TeecapEvalResult, ctx: &CodeGenContext) -> Option<TeecapEvalResult> {
        if let TeecapEvalResult::Variable(offset1, par1, var1) = self {
            if let TeecapEvalResult::Variable(offset2, None, var2) = other {
                let mid = ctx.resolve_var(*offset1, par1, var1)?;
                let offset = offset2 + mid.0;
                if let TeecapType::Struct(struct_name) = mid.1 {
                    Some(TeecapEvalResult::Variable(offset, Some(struct_name), var2.to_string()))
                } else{
                    None
                }
            } else{
                None
            }
        } else{
            None
        }
    }
}

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

struct CodeGenContext {
    cur_type: TeecapType,
    tag_count: u32,
    init_func: TeecapFunction,
    functions: Vec<TeecapFunction>,
    variables: Vec<TeecapScope>,
    reserved_stack_size: Vec<u32>,
    gprs: Vec<bool>,
    break_conts: Vec<(TeecapTag, TeecapTag)>, // break and continue stack
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
            cur_type: TeecapType::Int,
            tag_count : 0,
            functions: Vec::new(),
            variables: vec![TeecapScope::new()],
            reserved_stack_size: vec![0],
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

    fn add_var_to_scope(&mut self, field: TeecapField) {
        let offset = *self.reserved_stack_size.first().unwrap();
        *self.reserved_stack_size.first_mut().unwrap() += field.get_size(&self.struct_map);
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
            self.functions.first_mut().expect("Function list is empty!")
        } else {
            &mut self.init_func
        }.push_asm_unit(asm_unit);
    }

    fn enter_function(&mut self, func_name: &str) {
        self.in_func = true;
        self.functions.push(TeecapFunction::new(func_name));
    }

    fn exit_function(&mut self) {
        self.in_func = false;
    }

    fn grab_gpr(&mut self) -> Option<TeecapReg> {
        let some_n = self.gprs.iter().enumerate().find(|&(idx, &b)| !b).map(|x| x.0 as u8);
        if let &Some(n) = &some_n {
            self.gprs[n as usize] = true;
        }
        some_n.map(|x| TeecapReg::Gpr(x))
    }

    fn release_gpr(&mut self, reg: &TeecapReg) {
        if *reg != TEECAP_STACK_REG {
            if let &TeecapReg::Gpr(n) = reg {
                self.gprs[n as usize] = false;
            }
        }
    }

    fn gen_li(&mut self, reg: &TeecapReg, val: TeecapImm) {
        self.push_insn(TeecapInsn::Li(reg.clone(),
            val));
    }

    fn gen_li_alloc(&mut self, val: TeecapImm) -> TeecapReg {
        let reg = self.grab_gpr().expect("Gpr allocation for li failed!");
        self.gen_li(&reg, val);
        reg
    }

    fn gen_ld(&mut self, reg: &TeecapReg, offset: u32, parent_struct: &Option<String>, var_name: &str) {
        let cap_reg = self.gen_load_cap(offset, parent_struct, var_name).expect("Failed to obtain capability for ld!");
        self.push_insn(TeecapInsn::Ld(*reg, cap_reg));
        self.release_gpr(&cap_reg); // TODO: we should not reuse cap registers like this
        // it is necessary to write them back if the capability is linear
    }

    fn gen_ld_alloc(&mut self, offset: u32, parent_struct: &Option<String>, var_name: &str) -> TeecapReg {
        let reg = self.grab_gpr().expect("Gpr allocation for ld failed!");
        self.gen_ld(&reg, offset, parent_struct, var_name);
        reg
    }

    fn resolve_var(&self, offset: u32, parent_struct: &Option<String>, var_name: &str) -> Option<(u32, TeecapType)> {
        match parent_struct {
            Some(parent_name) => {
                let par = self.struct_map.get(parent_name)?;
                par.get_field(var_name).map(|(o, t)| (offset + o, t))
            }
            None => {
                self.find_var_in_scope(var_name).map(|(o, t)| (offset + o, t))
            }
        }
    }

    /**
     * Returns: the register that contains the capability which can be used for accessing
     * the specified variable (might fail if the variable cannot be accessed).
     * */
    fn gen_load_cap(&mut self, offset: u32, parent_struct: &Option<String>, var_name: &str) -> Option<TeecapReg> {
        // TODO: implement this
        let (r_offset, ftype) = self.resolve_var(offset, parent_struct, var_name)?;
        let offset_reg = self.gen_li_alloc(TeecapImm::Const(r_offset as u64));
        let cap_reg = TEECAP_STACK_REG; // let's pretend that it's always in the current inner-most scope
        self.push_insn(TeecapInsn::Scco(cap_reg, offset_reg));
        self.release_gpr(&offset_reg);
        Some(cap_reg)
    }

    fn gen_store(&mut self, offset: u32, parent_struct: &Option<String>, var_name: &str, val: &TeecapEvalResult) -> TeecapReg {
        let reg = val.to_register(self);
        let cap_reg = self.gen_load_cap(offset, parent_struct, var_name).expect("Unable to access variable through capabilities!");
        self.push_insn(TeecapInsn::Sd(cap_reg, reg));
        self.release_gpr(&cap_reg);
        reg
    }

    fn gen_store_drop_result(&mut self, offset: u32, parent_struct: &Option<String>, var_name: &str, val: &TeecapEvalResult) {
        let reg = self.gen_store(offset, parent_struct, var_name, val);
        self.release_gpr(&reg);
    }

    fn gen_assignment(&mut self, lhs: &TeecapEvalResult, rhs: &TeecapEvalResult) -> TeecapEvalResult {
        match &lhs {
            TeecapEvalResult::Variable(offset, parent_struct, var_name) => {
                TeecapEvalResult::Register(self.gen_store(*offset, parent_struct, var_name, rhs))
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
                TeecapInsn::Add(rd, rs)
            }
            TeecapOpAB::Minus => {
                TeecapInsn::Sub(rd, rs)
            }
            TeecapOpAB::And => {
                TeecapInsn::And(rd, rs)
            }
            TeecapOpAB::Or => {
                TeecapInsn::Or(rd, rs)
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
        for func in self.functions.iter() {
            func.print_code(&mut mem_offset);
        }
        // stack
        println!("{} {}", mem_offset, -1);
        println!(":<stack>");
        println!("$");
        println!("-1 -1");
    }

    fn drop_result(&mut self, res: &TeecapEvalResult) {
        if let TeecapEvalResult::Register(reg) = res {
            self.release_gpr(&reg);
        }
    }

    fn alloc_tag(&mut self) -> TeecapTag {
        let tag = TeecapTag(self.tag_count);
        self.tag_count += 1;
        tag
    }
    
    fn push_break_cont(&mut self, break_tag: TeecapTag, cont_tag: TeecapTag) {
        self.break_conts.push((break_tag, cont_tag));
    }

    fn pop_break_cont(&mut self) {
        self.break_conts.pop();
    }
    
    fn gen_jmp_tag(&mut self, tag: TeecapTag) {
        let target_reg = self.gen_li_alloc(TeecapImm::Tag(tag));
        self.push_insn(TeecapInsn::Jmp(target_reg));
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
        let res_reg = self.grab_gpr().expect("Failed to allocate gpr for eq!");
        self.push_insn(match op {
            TeecapOpRAB::Le => {
                TeecapInsn::Le(res_reg, lhs_reg, rhs_reg)
            }
            TeecapOpRAB::Lt => {
                TeecapInsn::Lt(res_reg, lhs_reg, rhs_reg)
            }
            TeecapOpRAB::Ge => {
                TeecapInsn::Le(res_reg, rhs_reg, lhs_reg)
            }
            TeecapOpRAB::Gt => {
                TeecapInsn::Lt(res_reg, rhs_reg, lhs_reg)
            }
            TeecapOpRAB::Eq => {
                TeecapInsn::Eq(res_reg, lhs_reg, rhs_reg)
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

    fn push_tag(&mut self, tag: TeecapTag) {
        self.push_asm_unit(TeecapAssemblyUnit::Tag(tag));
    }
    
    fn push_passthrough_asm(&mut self, s: String) {
        self.push_asm_unit(TeecapAssemblyUnit::Passthrough(s));
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
        TeecapEvalResult::Variable(0, None, self.name.clone())
    }
}

impl TeecapEvaluator for CallExpression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        let callee = self.callee.teecap_evaluate(ctx);
        let args: Vec<TeecapReg> = self.arguments.iter().map(|x| x.teecap_evaluate(ctx).to_register(ctx)).collect();
        let res = match &callee {
            TeecapEvalResult::Variable(offset, parent_struct, var_name) => {
                match var_name.as_str() {
                    "print" => {
                        ctx.push_insn(TeecapInsn::Out(args.first()
                            .expect("Missing argument for print!").clone()));
                        TeecapEvalResult::Const(0)
                    }
                    "exit" => {
                        ctx.push_insn(TeecapInsn::Halt);
                        TeecapEvalResult::Const(0)
                    }
                    _ => {
                        eprintln!("Function call not supported yet: {}", var_name);
                        TeecapEvalResult::Const(0)
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
        match self.operator.node {
            MemberOperator::Direct => {
                // a.b
                // for this type, directly compute the offset
                let lhs = self.expression.teecap_evaluate(ctx);
                let rhs = self.identifier.teecap_evaluate(ctx);
                lhs.join(&rhs, ctx).expect("Bad member expression!")
            }
            MemberOperator::Indirect => {
                eprintln!("Indirect member operator not supported yet!");
                TeecapEvalResult::Const(0)
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
        ctx.add_var_to_scope(TeecapField::new(name.to_string(), ctx.cur_type.clone()));

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
        ctx.gen_store_drop_result(0, &None, name, &init_val);
    }
}


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

#[derive(Clone, Debug)]
enum TeecapType {
    Int,
    Struct(String)
}

impl TeecapType {
    fn get_size(&self, struct_map: &TeecapStructMap) -> u32 {
        match self {
            TeecapType::Int => 1,
            TeecapType::Struct(struct_name) => {
                struct_map.get(struct_name).expect("Struct undefined!").size
            }
        }
    }
}


trait ToTeecapType {
    fn to_teecap_type(&self, ctx: &mut CodeGenContext) -> TeecapType;
}

struct TeecapField {
    name: String,
    field_type: TeecapType,
}

impl TeecapField {
    fn new(name: String, field_type: TeecapType) -> TeecapField {
        TeecapField { name: name, field_type: field_type }
    }
    fn parse(ctx: &mut CodeGenContext, struct_field: &StructField) -> TeecapField  {
        // only accept single specifier and declarator for now
        let name = get_decl_kind_name(&struct_field.declarators.first().expect("No declarator for struct fields!")
                                      .node.declarator.as_ref().expect("No declarator for struct fields!")
                                      .node.kind.node).expect("Struct field name missing!");
        let field_type = match &struct_field.specifiers.first().expect("No specifier for struct fields!").node {
            SpecifierQualifier::TypeSpecifier(t) => {
                t.to_teecap_type(ctx)
            }
            _ => {
                TeecapType::Int
            }
        };
        TeecapField {
            name: name.to_string(),
            field_type: field_type
        }
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
            for f in decl.iter() {
                match &f.node {
                    StructDeclaration::Field(field) => {
                        let field = TeecapField::parse(ctx, &field.node);
                        teecap_struct.add_field(field, &ctx.struct_map);
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

fn try_get_type(ctx: &mut CodeGenContext, decl_spec: &DeclarationSpecifier) -> Option<TeecapType> {
    match decl_spec {
        DeclarationSpecifier::TypeSpecifier(type_spec) => {
            Some(type_spec.to_teecap_type(ctx))
        }
        _ => {
            None
        }
    } 
}

impl TeecapEmitter for Declaration {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let ttype = self.specifiers.iter().map(|x| try_get_type(ctx, &x.node)).fold(None, |x, y| x.or(y))
            .unwrap_or(TeecapType::Int); // the default type is int
        ctx.cur_type = ttype;
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

        let reg_else = ctx.gen_li_alloc(TeecapImm::Tag(tag_else));
        ctx.push_insn(TeecapInsn::Jz(reg_else, reg_cond));
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
            let reg_end = ctx.gen_li_alloc(TeecapImm::Tag(tag_end));
            ctx.push_insn(TeecapInsn::Jmp(reg_end));
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
        let reg_end = ctx.gen_li_alloc(TeecapImm::Tag(tag_end));
        ctx.push_insn(TeecapInsn::Jz(reg_end, reg_cond));
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


impl TeecapEmitter for FunctionDefinition {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        let func_name = get_decl_kind_name(&self.declarator.node.kind.node)
            .expect("Bad function name!");
        // emit code for body
        ctx.enter_function(func_name);
        ctx.push_scope();
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
    for node in trans_unit.0.iter() {
        node.teecap_emit_code(&mut ctx);
    }
    // output the code
    ctx.print_code();
}

fn main() {
    let config = Config::default();
    match parse(&config, std::env::args().nth(1).expect("Missing source file name!")) {
        Ok(parser_result)  => {
            generate_code(&parser_result.unit);
        }
        Err(e) => {
            eprintln!("Parse error!");
            eprintln!("{:?}", e);
        }
    }
}

