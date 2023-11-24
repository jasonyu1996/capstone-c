use std::{collections::{HashSet, HashMap}, process::id};

use lang_c::{visit::Visit as ParserVisit, ast::{FunctionDefinition, TranslationUnit, DeclaratorKind, ParameterDeclaration, DerivedDeclarator, DeclarationSpecifier, TypeSpecifier, StructKind, StructDeclaration}, span::Span};
use crate::{dag::IRDAG, lang_defs::CaplanStruct};
use crate::dag_builder::IRDAGBuilder;
use crate::lang_defs::CaplanType;
use crate::utils::{GCed, new_gced};

/**
 * Function parameter
 */
#[derive(Debug)]
pub struct CaplanParam {
    pub name: String,
    pub ty: CaplanType
}

impl CaplanParam {
}

struct CaplanParamBuilder<'ast> {
    param: CaplanParam,
    globals: &'ast CaplanGlobalContext
}

impl<'ast> CaplanParamBuilder<'ast> {
    fn new(globals: &'ast CaplanGlobalContext) -> Self {
        Self {
            param: CaplanParam { name: String::new(), ty: CaplanType::Void },
            globals: globals
        }
    }

    fn build(&mut self, ast: &'ast ParameterDeclaration, span: &'ast Span) {
        self.visit_parameter_declaration(ast, span);
    }

    fn into_param(self) -> CaplanParam {
        self.param
    }
}

impl<'ast> ParserVisit<'ast> for CaplanParamBuilder<'ast> {
    fn visit_type_specifier(&mut self, type_specifier: &'ast lang_c::ast::TypeSpecifier, span: &'ast Span) {
        self.param.ty = CaplanType::from_ast_type(type_specifier, self.globals).unwrap();
    }

    fn visit_declarator_kind(&mut self, declarator_kind: &'ast DeclaratorKind, span: &'ast Span) {
        match declarator_kind {
            DeclaratorKind::Identifier(id_node) => {
                self.param.name = id_node.node.name.clone();
            },
            _ => {}
        }
    }

    fn visit_derived_declarator(
            &mut self,
            derived_declarator: &'ast lang_c::ast::DerivedDeclarator,
            span: &'ast Span,
        ) {
        match derived_declarator {
            DerivedDeclarator::Pointer(_) => {
                self.param.ty.make_pointer(false); // TODO: for now we only have nonlinear pointers
            },
            _ => {}
        }
    }
}

pub struct CaplanFunction {
    pub name: String,
    pub ret_type: CaplanType,
    pub params: Vec<CaplanParam>,
    pub dag: IRDAG
}

pub struct CaplanGlobalContext {
    pub func_decls: HashSet<String>,
    pub struct_defs: HashMap<String, GCed<CaplanStruct>>
}

impl CaplanGlobalContext {
    fn new() -> Self {
        Self {
            func_decls: HashSet::new(),
            struct_defs: HashMap::new()
        }
    }
}

struct CaplanFunctionBuilder<'ast> {
    func: CaplanFunction,
    globals: &'ast mut CaplanGlobalContext
}

impl<'ast> CaplanFunctionBuilder<'ast> {
    fn new(globals: &'ast mut CaplanGlobalContext) -> Self {
        Self {
            func: CaplanFunction {
                name: String::new(),
                ret_type: CaplanType::Int, // default is int
                params: Vec::new(),
                dag: IRDAG::new()
            },
            globals: globals
        }
    }

    fn build(&mut self, ast: &'ast FunctionDefinition, span: &'ast Span) {
        // check function signature
        let func_identifier = &ast.declarator.node.kind.node;
        match func_identifier {
            DeclaratorKind::Identifier(id_node) => {
                self.func.name = id_node.node.name.clone();
                self.globals.func_decls.insert(id_node.node.name.clone());
            },
            _ => {}
        }
        self.visit_function_definition(ast, span);
        let mut dag_builder = IRDAGBuilder::new(&*self.globals);
        dag_builder.build(ast, span, &self.func.params);
        self.func.dag = dag_builder.into_dag();
        eprintln!("Function name: {}", self.func.name);
        eprintln!("Function parameters: {:?}", self.func.params);
    }

    fn into_func(self) -> CaplanFunction {
        self.func
    }
}

pub struct CaplanTranslationUnit {
    pub globals: CaplanGlobalContext,
    pub functions: Vec<CaplanFunction>,
    in_global_context: bool,
    last_type: Option<CaplanType>,
    last_ident_name: Option<String>
}

impl<'ast> ParserVisit<'ast> for CaplanFunctionBuilder<'ast> {
    fn visit_function_declarator(
            &mut self,
            function_declarator: &'ast lang_c::ast::FunctionDeclarator,
            span: &'ast lang_c::span::Span,
        ) {
        for param_node in &function_declarator.parameters {
            let mut param_builder = CaplanParamBuilder::new(self.globals);
            param_builder.build(&param_node.node, &param_node.span);
            self.func.params.push(param_builder.into_param());
        }
    }
}

impl CaplanTranslationUnit {
    pub fn from_ast(ast: &TranslationUnit) -> Self {
        let mut res = CaplanTranslationUnit {
            globals: CaplanGlobalContext::new(),
            functions: Vec::new(),
            in_global_context: true,
            last_type: None,
            last_ident_name: None
        };
        res.visit_translation_unit(ast);
        res
    }
}

impl<'ast> ParserVisit<'ast> for CaplanTranslationUnit {
    fn visit_function_definition(
            &mut self,
            function_definition: &'ast lang_c::ast::FunctionDefinition,
            span: &'ast lang_c::span::Span,
        ) {
        let mut func_builder = CaplanFunctionBuilder::new(&mut self.globals);
        func_builder.build(function_definition, span);
        self.functions.push(func_builder.into_func());
    }

    // fn visit_declaration(
    //         &mut self,
    //         declaration: &'ast lang_c::ast::Declaration,
    //         span: &'ast lang_c::span::Span,
    //     ) {
        // let is_function_decl = declaration.specifiers.iter()
        //     .find(|x| matches!(x.node, DeclarationSpecifier::Function(_))).is_some();
    // }

    fn visit_derived_declarator(
            &mut self,
            derived_declarator: &'ast DerivedDeclarator,
            span: &'ast Span,
        ) {
        match derived_declarator {
            DerivedDeclarator::KRFunction(identifiers) => {
                for ident in identifiers.iter() {
                    self.globals.func_decls.insert(ident.node.name.clone());
                }
            }
            _ => panic!("Unsupported declarator")
        }
    }

    fn visit_struct_type(&mut self, struct_type: &'ast lang_c::ast::StructType, span: &'ast Span) {
        assert!(self.in_global_context);
        assert!(matches!(struct_type.kind.node, StructKind::Struct), "Union is not supported");
        let struct_name = struct_type.identifier.as_ref().expect("Anonymous struct not supported").node.name.clone();
        let struct_def = new_gced(CaplanStruct::new());
        assert!(self.globals.struct_defs.insert(struct_name, struct_def.clone()).is_none(), "Duplicate struct defs");
        let struct_decls = struct_type.declarations.as_ref().expect("Struct decl cannot be empty");
        self.in_global_context = false;
        for struct_decl in struct_decls.iter() {
            if let StructDeclaration::Field(field) = &struct_decl.node {
                self.visit_struct_field(&field.node, &field.span);
                struct_def.borrow_mut().add_field(self.last_ident_name.as_ref().unwrap(), self.last_type.take().unwrap());
            } else {
                panic!("Static assertion not supported");
            }
        }
        self.in_global_context = true;

        eprintln!("Struct added with {} fields", struct_def.borrow().fields.len());
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
        self.last_ident_name = Some(identifier.name.clone());
    }

    fn visit_type_specifier(&mut self, type_specifier: &'ast TypeSpecifier, span: &'ast Span) {
        match type_specifier {
            TypeSpecifier::Struct(struct_type) => {
                if self.in_global_context {
                    self.visit_struct_type(&struct_type.node, &struct_type.span);
                } else {
                    self.last_type = CaplanType::from_ast_type(type_specifier, &self.globals);
                }
            }
            _ => self.last_type = CaplanType::from_ast_type(type_specifier, &self.globals)
        }
    }
    
}
