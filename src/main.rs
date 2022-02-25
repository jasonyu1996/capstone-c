use std::fmt::Display;
/**
 * Features omitted: no distinguishing different int sizes
 *
 * */


use std::{iter::Map, collections::HashMap};
use std::str::FromStr;

use lang_c::{driver::{Config, parse}, ast::{TranslationUnit, FunctionDefinition,
    StaticAssert, Declarator, Declaration, DeclaratorKind, Statement, BlockItem, InitDeclarator, Integer, Initializer, Expression, Constant}};
use lang_c::ast::{ExternalDeclaration, BinaryOperatorExpression, BinaryOperator, Identifier, IfStatement, WhileStatement, CallExpression};
use lang_c::span::Node;

const TEECAP_GPR_N: usize = 32; // number of general-purpose registers
const TEECAP_STACK_REG: TeecapReg = TeecapReg::Gpr(0); // r0 is used for storing the stack capability

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
    Add(TeecapReg, TeecapReg),
    Jnz(TeecapReg, TeecapReg),
    Jz(TeecapReg, TeecapReg),
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
            TeecapInsn::Add(rd, rs) => {
                write!(f, "add {} {}", rd, rs)
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
            _ => {
                write!(f, "<und>")
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TeecapTag(u32);

enum TeecapAssemblyUnit {
    TeecapTag(TeecapTag),
    TeecapInstruction(TeecapInsn)
}

impl Display for TeecapTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":<{}>", self.0)
    }
}

impl Display for TeecapAssemblyUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapAssemblyUnit::TeecapTag(tag) => {
                tag.fmt(f)
            }
            TeecapAssemblyUnit::TeecapInstruction(insn) => {
                insn.fmt(f)
            }
        }
    }
}

struct TeecapFunction {
    code: Vec<TeecapAssemblyUnit>
}

impl TeecapFunction {
    fn new() -> TeecapFunction {
        TeecapFunction {
            code: Vec::new()
        }
    }

    fn push_asm_unit(&mut self, asm_unit: TeecapAssemblyUnit) {
        self.code.push(asm_unit);
    }

    fn print_code(&self) {
        for asm_unit in self.code.iter() {
            println!("{}", asm_unit);
        }
    }
}

//#[derive(Debug)]
//struct TeecapVariable {
    //name: String,
    //offset: u32 // offset of the variable in the stack frame
    ////init_value: TeecapInt // only supporting unsigned 64-bit
//}

type TeecapScope = HashMap<String, u32>;

#[derive(Debug)]
enum TeecapEvalResult {
    Const(TeecapInt),
    Register(TeecapReg),
    Variable(String)
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
            TeecapEvalResult::Variable(r_var_name) => {
                // load the variable
                ctx.gen_ld_alloc(r_var_name)
            }
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
    tag_count: u32,
    init_func: TeecapFunction,
    functions: Vec<TeecapFunction>,
    variables: Vec<TeecapScope>,
    reserved_stack_size: Vec<u32>,
    gprs: Vec<bool>,
    break_conts: Vec<(TeecapTag, TeecapTag)>, // break and continue stack
    in_func: bool
}


impl CodeGenContext {
    fn new() -> CodeGenContext {
        let mut instance = CodeGenContext {
            tag_count : 0,
            functions: Vec::new(),
            variables: vec![TeecapScope::new()],
            reserved_stack_size: vec![0],
            gprs: vec![false; TEECAP_GPR_N],
            in_func: false,
            init_func: TeecapFunction::new(),
            break_conts: Vec::new()
        };
        if let TeecapReg::Gpr(n) = TEECAP_STACK_REG {
            instance.gprs[n as usize] = true; // reserve the gpr for the stack cap
        }
        instance
    }

    fn add_var_to_scope(&mut self, var_name: String) {
        let offset = *self.reserved_stack_size.first().unwrap();
        *self.reserved_stack_size.first_mut().unwrap() += 1;
        self.variables.first_mut().unwrap().insert(var_name, offset);
    }

    fn find_var_in_scope(&self, var_name: &str) -> Option<u32> {
        self.variables.iter().map(|x| x.get(var_name)).fold(None, |x, y| x.or(y.map(|v| *v)))
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

    fn enter_function(&mut self) {
        self.in_func = true;
        self.functions.push(TeecapFunction::new());
    }

    fn exit_function(&mut self) {
        self.in_func = false;
    }

    fn grab_gpr(&mut self) -> Option<u8> {
        let some_n = self.gprs.iter().enumerate().find(|&(idx, &b)| !b).map(|x| x.0 as u8);
        if let &Some(n) = &some_n {
            self.gprs[n as usize] = true;
        }
        some_n
    }

    fn release_gpr(&mut self, n: u8) {
        self.gprs[n as usize] = false;
    }

    fn release_register(&mut self, reg: &TeecapReg) {
        if *reg != TEECAP_STACK_REG {
            if let &TeecapReg::Gpr(n) = reg {
                self.release_gpr(n);
            }
        }
    }

    fn gen_li(&mut self, reg: &TeecapReg, val: TeecapImm) {
        self.push_insn(TeecapInsn::Li(reg.clone(),
            val));
    }

    fn gen_li_alloc(&mut self, val: TeecapImm) -> TeecapReg {
        let reg = TeecapReg::Gpr(self.grab_gpr().expect("Gpr allocation failed!"));
        self.gen_li(&reg, val);
        reg
    }

    fn gen_ld(&mut self, reg: &TeecapReg, var_name: &str) {
        let cap_reg = self.gen_load_cap(var_name).expect("Failed to obtain capability for ld!");
        self.push_insn(TeecapInsn::Ld(*reg, cap_reg));
        self.release_register(&cap_reg); // TODO: we should not reuse cap registers like this
        // it is necessary to write them back if the capability is linear
    }

    fn gen_ld_alloc(&mut self, var_name: &str) -> TeecapReg {
        let reg = TeecapReg::Gpr(self.grab_gpr().expect("Gpr allocation for ld failed!"));
        self.gen_ld(&reg, var_name);
        reg
    }

    /**
     * Returns: the register that contains the capability which can be used for accessing
     * the specified variable (might fail if the variable cannot be accessed).
     * */
    fn gen_load_cap(&mut self, var_name: &str) -> Option<TeecapReg> {
        // TODO: implement this
        let offset = self.find_var_in_scope(var_name)?;
        let offset_reg = self.gen_li_alloc(TeecapImm::Const(offset as u64));
        let cap_reg = TEECAP_STACK_REG; // let's pretend that it's always in the current inner-most scope
        self.push_insn(TeecapInsn::Scc(cap_reg, offset_reg));
        self.release_register(&offset_reg);
        Some(cap_reg)
    }

    fn gen_store(&mut self, var_name: &str, val: &TeecapEvalResult) -> TeecapReg {
        let reg = val.to_register(self);
        let cap_reg = self.gen_load_cap(var_name).expect("Unable to access variable through capabilities!");
        self.push_insn(TeecapInsn::Sd(cap_reg, reg));
        self.release_register(&cap_reg);
        reg
    }

    fn gen_store_drop_result(&mut self, var_name: &str, val: &TeecapEvalResult) {
        let reg = self.gen_store(var_name, val);
        self.release_register(&reg);
    }

    fn gen_assignment(&mut self, lhs: &TeecapEvalResult, rhs: &TeecapEvalResult) -> TeecapEvalResult {
        match lhs {
            TeecapEvalResult::Variable(var_name) => {
                TeecapEvalResult::Register(self.gen_store(var_name, rhs))
            }
            _ => {
                eprintln!("Unsupported lvalue for assignment: {:?}", lhs);
                TeecapEvalResult::Const(0)
            }
        }
    }

    fn gen_plus(&mut self, lhs: &TeecapEvalResult, rhs: &TeecapEvalResult) -> TeecapEvalResult {
        let rd = lhs.to_register(self);
        let rs = rhs.to_register(self);
        self.push_insn(TeecapInsn::Add(rd, rs));
        TeecapEvalResult::Register(rd)
    }

    fn print_code(&self) {
        self.init_func.print_code();
        for func in self.functions.iter() {
            func.print_code();
        }
    }

    fn drop_result(&mut self, res: &TeecapEvalResult) {
        if let TeecapEvalResult::Register(reg) = res {
            self.release_register(&reg);
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
        self.release_register(&target_reg);
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

    fn push_insn(&mut self, insn: TeecapInsn) {
        self.push_asm_unit(TeecapAssemblyUnit::TeecapInstruction(insn));
    }

    fn push_tag(&mut self, tag: TeecapTag) {
        self.push_asm_unit(TeecapAssemblyUnit::TeecapTag(tag));
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
                ctx.gen_plus(&lhs, &rhs)
            }
            _ => {
                TeecapEvalResult::Const(0)
            }
        }
    }
}

impl TeecapEvaluator for Identifier {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        TeecapEvalResult::Variable(self.name.clone())
    }
}

impl TeecapEvaluator for CallExpression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        let callee = self.callee.teecap_evaluate(ctx);
        let args: Vec<TeecapReg> = self.arguments.iter().map(|x| x.teecap_evaluate(ctx).to_register(ctx)).collect();
        let res = match &callee {
            TeecapEvalResult::Variable(var_name) => {
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
            ctx.release_register(&arg);
        }
        res
    }
}


impl TeecapEvaluator for Expression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        match &self {
            Expression::Constant(con) => {
                con.teecap_evaluate(ctx)
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
        ctx.add_var_to_scope(name.to_string());

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
        ctx.gen_store_drop_result(name, &init_val);
    }
}

impl TeecapEmitter for Declaration {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        // omitting the specifiers for now
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
        ctx.release_register(&reg_cond);
        ctx.release_register(&reg_else);
        
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
            ctx.release_register(&reg_end);

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
        ctx.release_register(&reg_end);
        ctx.release_register(&reg_cond);

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
        ctx.enter_function();
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

