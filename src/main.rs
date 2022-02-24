/**
 * Features omitted: no distinguishing different int sizes
 *
 * */


use std::{iter::Map, collections::HashMap};
use std::str::FromStr;

use lang_c::{driver::{Config, parse}, ast::{TranslationUnit, FunctionDefinition,
    StaticAssert, Declarator, Declaration, DeclaratorKind, Statement, BlockItem, InitDeclarator, Integer, Initializer, Expression, Constant}};
use lang_c::ast::ExternalDeclaration;
use lang_c::span::Node;

const TEECAP_GPR_N: usize = 32; // number of general-purpose registers

type TeecapInt = u64;


enum TeecapReg {
    PC, ID, EPC, RET, GPR(u8)
}

enum TeecapInsn {
    OP_LI(TeecapReg, TeecapInt)
}

struct TeecapTag(i32);

enum TeecapAssemblyUnit {
    TEECAP_TAG(TeecapTag),
    TEECAP_INSTRUCTION(TeecapInsn)
}

struct TeecapFunction {
    code: Vec<TeecapAssemblyUnit>
}

//#[derive(Debug)]
//struct TeecapVariable {
    //name: String,
    //offset: u32 // offset of the variable in the stack frame
    ////init_value: TeecapInt // only supporting unsigned 64-bit
//}

type TeecapScope = HashMap<String, u32>;

enum TeecapEvalResult {
    Const(TeecapInt),
    Register(TeecapReg)
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
    functions: Vec<TeecapFunction>,
    variables: Vec<TeecapScope>,
    reserved_stack_size: Vec<u32>,
    gprs: Vec<bool>,
}

impl CodeGenContext {
    fn new() -> CodeGenContext {
        CodeGenContext {
            tag_count : 0,
            functions: Vec::new(),
            variables: vec![TeecapScope::new()],
            reserved_stack_size: vec![0],
            gprs: vec![false; TEECAP_GPR_N]
        }
    }
}

impl CodeGenContext {
    fn add_var_to_scope(&mut self, var_name: String) {
        let offset = *self.reserved_stack_size.first().unwrap();
        *self.reserved_stack_size.first_mut().unwrap() += 1;
        self.variables.first_mut().unwrap().insert(var_name, offset);
    }

    fn pop_scope(&mut self) {
        self.variables.pop();
    }

    fn push_scope(&mut self, scope: TeecapScope) {
        self.variables.push(scope);
    }

    fn grab_gpr(&mut self) -> Option<u8> {
        let some_n = self.gprs.iter().enumerate().find(|&(idx, &b)| b).map(|x| x.0 as u8);
        if let &Some(n) = &some_n {
            self.gprs[n as usize] = true;
        }
        some_n
    }

    fn release_gpr(&mut self, n: u8) {
        self.gprs[n as usize] = false;
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

impl TeecapEvaluator for Expression {
    fn teecap_evaluate(&self, ctx: &mut CodeGenContext) -> TeecapEvalResult {
        match &self {
            Expression::Constant(con) => {
                con.teecap_evaluate(ctx)
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

fn teecap_gen_assign(var_name: &str, ctx: &mut CodeGenContext, val: &TeecapEvalResult) {
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
                        eprintln!("Literal format unsupported!");
                        TeecapEvalResult::Const(0)
                    }
                }
            }
            None => {
                eprintln!("Literal format unsupported!");
                TeecapEvalResult::Const(0)
            }
        };
        teecap_gen_assign(name, ctx, &init_val);
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

impl TeecapEmitter for Statement {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        match &self {
            Statement::Compound(v) => {
                for n in v.iter() {
                    n.teecap_emit_code(ctx);
                }
            }
            _ => {
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
        ctx.push_scope(TeecapScope::new());
        self.statement.teecap_emit_code(ctx);
        ctx.pop_scope();
    }
}

impl TeecapEmitter for ExternalDeclaration {
    fn teecap_emit_code(&self, ctx: &mut CodeGenContext) {
        match self {
            ExternalDeclaration::Declaration(n) => {
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

