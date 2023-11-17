use lang_c::{visit::Visit as ParserVisit, ast::{FunctionDefinition, TranslationUnit, DeclaratorKind, ParameterDeclaration, DerivedDeclarator}, span::Span};
use crate::dag::IRDAG;
use crate::lang_defs::CaplanType;

/**
 * Function parameter
 */
#[derive(Debug)]
pub struct CaplanParam {
    name: String,
    ty: CaplanType
}

impl CaplanParam {
    fn from_ast(ast: &ParameterDeclaration, span: &Span) -> Self {
        let mut res = CaplanParam {
            name: String::new(),
            ty: CaplanType::Void
        };
        res.visit_parameter_declaration(ast, span);
        res
    }
}

impl<'ast> ParserVisit<'ast> for CaplanParam {
    fn visit_type_specifier(&mut self, type_specifier: &'ast lang_c::ast::TypeSpecifier, span: &'ast Span) {
        self.ty = CaplanType::from_ast_type(type_specifier).unwrap();
    }

    fn visit_declarator_kind(&mut self, declarator_kind: &'ast DeclaratorKind, span: &'ast Span) {
        match declarator_kind {
            DeclaratorKind::Identifier(id_node) => {
                self.name = id_node.node.name.clone();
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
                self.ty.make_pointer(false); // TODO: for now we only have nonlinear pointers
            },
            _ => {}
        }
    }
}

pub struct CaplanFunction {
    name: String,
    ret_type: CaplanType,
    params: Vec<CaplanParam>,
    dag: IRDAG
}

pub struct CaplanTranslationUnit {
    functions: Vec<CaplanFunction>
}

impl CaplanFunction {
    fn from_ast(ast: &FunctionDefinition, span: &Span) -> Self {
        let mut res = CaplanFunction {
            name: String::new(),
            ret_type: CaplanType::Int, // default is int
            params: Vec::new(),
            dag: IRDAG::from_ast(ast, span)
        };
        // check function signature
        let func_identifier = &ast.declarator.node.kind.node;
        match func_identifier {
            DeclaratorKind::Identifier(id_node) => {
                res.name = id_node.node.name.clone()
            },
            _ => {}
        }
        res.visit_function_definition(ast, span);
        eprintln!("Function name: {}", res.name);
        eprintln!("Function parameters: {:?}", res.params);
        res
    }
}

impl<'ast> ParserVisit<'ast> for CaplanFunction {
    fn visit_function_declarator(
            &mut self,
            function_declarator: &'ast lang_c::ast::FunctionDeclarator,
            span: &'ast lang_c::span::Span,
        ) {
        for param_node in &function_declarator.parameters {
            self.params.push(CaplanParam::from_ast(&param_node.node, &param_node.span))
        }
    }
}

impl CaplanTranslationUnit {
    pub fn from_ast(ast: &TranslationUnit) -> Self {
        let mut res = CaplanTranslationUnit {
            functions: Vec::new()
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
        self.functions.push(
            CaplanFunction::from_ast(function_definition, span)
        );
    }
}
