use std::{collections::{HashSet, HashMap}, process::id};

use lang_c::{visit::Visit as ParserVisit, ast::{FunctionDefinition, TranslationUnit, DeclaratorKind, ParameterDeclaration, DerivedDeclarator, DeclarationSpecifier, TypeSpecifier, StructKind, StructDeclaration, Extension, Declaration}, span::Span};
use crate::{dag::IRDAG, lang_defs::{CaplanStruct, try_modify_type_with_attr, CaplanEntryType, try_apply_func_attr, CaplanReentryType}, target_conf::CaplanTargetConf};
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

struct CaplanParamBuilder<'ctx> {
    param: CaplanParam,
    attrs: Vec<String>,
    globals: &'ctx CaplanGlobalContext
}

impl<'ctx> CaplanParamBuilder<'ctx> {
    fn new(globals: &'ctx CaplanGlobalContext) -> Self {
        Self {
            param: CaplanParam { name: String::new(), ty: CaplanType::Void },
            attrs: Vec::new(),
            globals: globals
        }
    }

    fn build(&mut self, ast: &'ctx ParameterDeclaration, span: &'ctx Span) {
        self.visit_parameter_declaration(ast, span);
        for a in self.attrs.iter() {
            assert!(try_modify_type_with_attr(&mut self.param.ty, a), "Unrecognised type attribute \"{}\" for type {:?}", a, self.param.ty);
        }
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

    fn visit_extension(&mut self, extension: &'ast lang_c::ast::Extension, span: &'ast Span) {
        match extension {
            Extension::Attribute(attr) => self.attrs.push(attr.name.node.clone()),
            _ => ()
        }
    }

    fn visit_derived_declarator(
            &mut self,
            derived_declarator: &'ast lang_c::ast::DerivedDeclarator,
            span: &'ast Span,
        ) {
        self.param.ty.decorate_from_ast(derived_declarator, &self.globals.target_conf);
    }
}

pub struct CaplanFunction {
    pub name: String,
    pub entry_type: CaplanEntryType,
    pub reentry_type: CaplanReentryType,
    pub is_naked: bool,
    pub ret_type: CaplanType,
    pub params: Vec<CaplanParam>,
    pub locals: HashMap<String, CaplanType>,
    pub dag: IRDAG
}

pub struct CaplanGlobalContext {
    pub target_conf: CaplanTargetConf,
    pub func_decls: HashMap<String, CaplanType>,
    pub struct_defs: HashMap<String, GCed<CaplanStruct>>,
    pub global_vars: Vec<CaplanType>,
    pub global_vars_to_ids: HashMap<String, usize>
}

impl CaplanGlobalContext {
    fn new(target_conf: CaplanTargetConf) -> Self {
        Self {
            target_conf: target_conf,
            func_decls: HashMap::new(),
            struct_defs: HashMap::new(),
            global_vars: Vec::new(),
            global_vars_to_ids: HashMap::new()
        }
    }
}

struct CaplanFunctionBuilder<'ctx> {
    func: CaplanFunction,
    globals: &'ctx mut CaplanGlobalContext,
    ret_type_attrs: Vec<String>,
    in_declaration_specifier: bool
}

impl<'ctx> CaplanFunctionBuilder<'ctx> {
    fn new(globals: &'ctx mut CaplanGlobalContext) -> Self {
        Self {
            func: CaplanFunction {
                name: String::new(),
                entry_type: CaplanEntryType::default(),
                ret_type: CaplanType::Int, // default is int
                reentry_type: CaplanReentryType::default(),
                is_naked: false,
                params: Vec::new(),
                dag: IRDAG::new(),
                locals: HashMap::new()
            },
            globals: globals,
            ret_type_attrs: Vec::new(),
            in_declaration_specifier: false
        }
    }

    fn build(&mut self, ast: &'ctx FunctionDefinition, span: &'ctx Span) {
        // check function signature
        let func_identifier = &ast.declarator.node.kind.node;
        match func_identifier {
            DeclaratorKind::Identifier(id_node) => {
                self.func.name = id_node.node.name.clone();
            },
            _ => {}
        }
        self.visit_function_definition(ast, span);
        for attr in self.ret_type_attrs.iter() {
            assert!(try_modify_type_with_attr(&mut self.func.ret_type, attr) ||
                try_apply_func_attr(attr, &mut self.func));
        }
        self.globals.func_decls.insert(self.func.name.clone(), self.func.ret_type.clone());

        eprintln!("Function name: {}", self.func.name);
        eprintln!("Function entry type: {:?}", self.func.entry_type);
        eprintln!("Function return type: {:?}", self.func.ret_type);
        eprintln!("Function parameters: {:?}", self.func.params);

        let mut dag_builder = IRDAGBuilder::new(&*self.globals);
        dag_builder.build(ast, span, &self.func.params);
        (self.func.dag, self.func.locals) = dag_builder.into_dag();
    }

    fn into_func(self) -> CaplanFunction {
        self.func
    }
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

    fn visit_type_specifier(&mut self, type_specifier: &'ast TypeSpecifier, span: &'ast Span) {
        self.func.ret_type = CaplanType::from_ast_type(type_specifier, &self.globals).unwrap();
    }

    fn visit_function_definition(
            &mut self,
            function_definition: &'ast FunctionDefinition,
            span: &'ast Span,
        ) {
        self.in_declaration_specifier = true;
        function_definition.specifiers.iter().for_each(|specifier| self.visit_declaration_specifier(&specifier.node, &specifier.span));
        self.in_declaration_specifier = false;
        self.visit_declarator(&function_definition.declarator.node, &function_definition.declarator.span);
        function_definition.declarations.iter().for_each(|declaration| self.visit_declaration(&declaration.node, &declaration.span));
        self.visit_statement(&function_definition.statement.node, &function_definition.statement.span);
    }

    fn visit_attribute(&mut self, attribute: &'ast lang_c::ast::Attribute, span: &'ast Span) {
        if self.in_declaration_specifier {
            // we only deal with attributes for the whole declaration, which is considered as applying to the return type
            self.ret_type_attrs.push(attribute.name.node.clone());
        }
    }

    fn visit_derived_declarator(
            &mut self,
            derived_declarator: &'ast DerivedDeclarator,
            span: &'ast Span,
        ) {
        match derived_declarator {
            DerivedDeclarator::Function(function_declarator) => self.visit_function_declarator(&function_declarator.node, &function_declarator.span),
            _ => self.func.ret_type.decorate_from_ast(derived_declarator, &self.globals.target_conf)
        }
    }

}
pub struct CaplanTranslationUnit {
    pub globals: CaplanGlobalContext,
    pub functions: Vec<CaplanFunction>,
    in_global_context: bool,
    last_type: Option<CaplanType>,
    last_ident_names: Vec<String>,
    attrs: Vec<String>
}


impl CaplanTranslationUnit {
    pub fn from_ast(ast: &TranslationUnit, target_conf: CaplanTargetConf) -> Self {
        let mut res = CaplanTranslationUnit {
            globals: CaplanGlobalContext::new(target_conf),
            functions: Vec::new(),
            in_global_context: true,
            last_type: None,
            last_ident_names: Vec::new(),
            attrs: Vec::new()
        };
        res.visit_translation_unit(ast);
        assert!(res.functions.iter().filter(
            |f| matches!(f.entry_type, CaplanEntryType::CrossDom)
        ).count() <= 1, "A translation unit can have at most one domain entry function.");
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
                    panic!("KR function not supported");
                }
            }
            _ => self.last_type.as_mut().unwrap().decorate_from_ast(derived_declarator, &self.globals.target_conf)
        }
    }

    fn visit_struct_type(&mut self, struct_type: &'ast lang_c::ast::StructType, span: &'ast Span) {
        assert!(self.in_global_context);
        assert!(matches!(struct_type.kind.node, StructKind::Struct), "Union is not supported");
        let struct_name = struct_type.identifier.as_ref().expect("Anonymous struct not supported").node.name.clone();
        if let Some(struct_decls) = struct_type.declarations.as_ref() {
            let struct_def = new_gced(CaplanStruct::new());
            assert!(self.globals.struct_defs.insert(struct_name, struct_def.clone()).is_none(), "Duplicate struct defs");
            self.in_global_context = false;
            for struct_decl in struct_decls.iter() {
                if let StructDeclaration::Field(field) = &struct_decl.node {
                    assert!(self.last_ident_names.is_empty());
                    self.visit_struct_field(&field.node, &field.span);
                    let ident_names = std::mem::replace(&mut self.last_ident_names, Vec::new());
                    let mut last_type = self.last_type.take().unwrap();
                    for attr in self.attrs.iter() {
                        assert!(try_modify_type_with_attr(&mut last_type, attr), "Unable to apply type attribute {}", attr);
                    }
                    self.attrs.clear();
                    for ident_name in ident_names {
                        struct_def.borrow_mut().add_field(&ident_name, last_type.clone(), &self.globals.target_conf);
                    }
                } else {
                    panic!("Static assertion not supported");
                }
            }
            self.in_global_context = true;

            eprintln!("Struct added with {} fields", struct_def.borrow().fields.len());

            self.last_type = Some(CaplanType::StructRef(struct_def));
        } else {
            self.last_type = Some(CaplanType::StructRef(self.globals.struct_defs.get(&struct_name).unwrap().clone()));
        }
    }

    fn visit_extension(&mut self, extension: &'ast Extension, span: &'ast Span) {
        match extension {
            Extension::Attribute(attr) => self.attrs.push(attr.name.node.clone()),
            _ => ()
        }
    }

    fn visit_identifier(&mut self, identifier: &'ast lang_c::ast::Identifier, span: &'ast Span) {
        self.last_ident_names.push(identifier.name.clone());
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

    fn visit_declaration(&mut self, declaration: &'ast lang_c::ast::Declaration, span: &'ast Span) {
        for specifier in declaration.specifiers.iter() {
            self.visit_declaration_specifier(&specifier.node, &specifier.span);
        }
        assert!(self.last_ident_names.is_empty());
        for declarator in declaration.declarators.iter() {
            self.visit_init_declarator(&declarator.node, &declarator.span);
        }
        let mut ty = self.last_type.take().unwrap();
        for attr in self.attrs.iter() {
            assert!(try_modify_type_with_attr(&mut ty, attr), "Unable to apply type attribute {}", attr);
        }
        self.attrs.clear();

        let ident_names = std::mem::replace(&mut self.last_ident_names, Vec::new());
        // ident_names might be empty in the case of struct
        for ident_name in ident_names {
            eprintln!("Global variable {} with type {:?}", ident_name, ty);
            let var_index = self.globals.global_vars.len();
            self.globals.global_vars.push(ty.clone());
            assert!(self.globals.global_vars_to_ids.insert(ident_name, var_index).is_none(), "Duplicate global variable declaration");
        }
    }
    
}
