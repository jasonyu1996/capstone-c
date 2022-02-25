use std::fmt::Display;
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
const TEECAP_STACK_REG: TeecapReg = TeecapReg::GPR(0); // r0 is used for storing the stack capability

type TeecapInt = u64;


#[derive(Clone, Copy, PartialEq)]
enum TeecapReg {
    PC, ID, EPC, RET, GPR(u8)
}

impl Display for TeecapReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
           TeecapReg::PC => {
               write!(f, "pc")
           }
           TeecapReg::ID => {
               write!(f, "id")
           }
           TeecapReg::EPC => {
               write!(f, "epc")
           }
           TeecapReg::RET => {
               write!(f, "ret")
           }
           TeecapReg::GPR(n) => {
               write!(f, "r{}", n)
           }
        }
    }
}

#[derive(Clone, Copy)]
enum TeecapInsn {
    OP_LI(TeecapReg, TeecapInt),
    OP_SD(TeecapReg, TeecapReg),
    OP_LD(TeecapReg, TeecapReg),
    OP_SCC(TeecapReg, TeecapReg),
}

impl Display for TeecapInsn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapInsn::OP_LI(r, v) => {
                write!(f, "li {} {}", r, v)
            }
            TeecapInsn::OP_SD(rd, rs) => {
                write!(f, "sd {} {}", rd, rs)
            }
            TeecapInsn::OP_LD(rd, rs) => {
                write!(f, "ld {} {}", rd, rs)
            }
            TeecapInsn::OP_SCC(rd, rs) => {
                write!(f, "scc {} {}", rd, rs)
            }
        }
    }
}

#[derive(Clone, Copy)]
struct TeecapTag(i32);

enum TeecapAssemblyUnit {
    TEECAP_TAG(TeecapTag),
    TEECAP_INSTRUCTION(TeecapInsn)
}

impl Display for TeecapAssemblyUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TeecapAssemblyUnit::TEECAP_TAG(tag) => {
                // TODO: tag printing
                write!(f, "")
            }
            TeecapAssemblyUnit::TEECAP_INSTRUCTION(insn) => {
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
    init_func: TeecapFunction,
    functions: Vec<TeecapFunction>,
    variables: Vec<TeecapScope>,
    reserved_stack_size: Vec<u32>,
    gprs: Vec<bool>,
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
            init_func: TeecapFunction::new()
        };
        if let TeecapReg::GPR(n) = TEECAP_STACK_REG {
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

    fn push_scope(&mut self, scope: TeecapScope) {
        self.variables.push(scope);
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
            if let &TeecapReg::GPR(n) = reg {
                self.release_gpr(n);
            }
        }
    }

    fn gen_li(&mut self, reg: &TeecapReg, val: TeecapInt) {
        self.push_asm_unit(TeecapAssemblyUnit::TEECAP_INSTRUCTION(TeecapInsn::OP_LI(reg.clone(), val)));
    }

    fn gen_li_alloc(&mut self, val: TeecapInt) -> TeecapReg {
        let reg = TeecapReg::GPR(self.grab_gpr().expect("GPR allocation failed!"));
        self.gen_li(&reg, val);
        reg
    }

    /**
     * Returns: the register that contains the capability which can be used for accessing
     * the specified variable (might fail if the variable cannot be accessed).
     * */
    fn gen_load_cap(&mut self, var_name: &str) -> Option<TeecapReg> {
        // TODO: implement this
        let offset = self.find_var_in_scope(var_name)?;
        let offset_reg = self.gen_li_alloc(offset as u64);
        let cap_reg = TEECAP_STACK_REG; // let's pretend that it's always in the current inner-most scope
        self.push_asm_unit(TeecapAssemblyUnit::TEECAP_INSTRUCTION(TeecapInsn::OP_SCC(cap_reg, offset_reg)));
        self.release_register(&offset_reg);
        Some(cap_reg)
    }

    fn gen_store(&mut self, var_name: &str, val: &TeecapEvalResult) {
        let reg : TeecapReg = match val {
            &TeecapEvalResult::Const(n) => {
                self.gen_li_alloc(n)
            }
            TeecapEvalResult::Register(r) => {
                r.clone()
            }
        };
        let cap_reg = self.gen_load_cap(var_name).expect("Unable to access variable through capabilities!");
        self.push_asm_unit(TeecapAssemblyUnit::TEECAP_INSTRUCTION(TeecapInsn::OP_SD(cap_reg, reg)));
        self.release_register(&cap_reg);
        self.release_register(&reg);
    }


    fn print_code(&self) {
        self.init_func.print_code();
        for func in self.functions.iter() {
            func.print_code();
        }
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
        ctx.gen_store(name, &init_val);
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
        ctx.enter_function();
        ctx.push_scope(TeecapScope::new());
        self.statement.teecap_emit_code(ctx);
        ctx.pop_scope();
        ctx.exit_function();
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

