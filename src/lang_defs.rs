use lang_c::ast::TypeSpecifier as ASTTypeSpecifier;

#[derive(Debug, Clone)]
pub enum CaplanType {
    Void,
    Int,
    Dom,
    LinPtr(Box<CaplanType>),
    NonlinPtr(Box<CaplanType>),
    Array
}

impl CaplanType {
    pub fn from_ast_type(ast_type: &ASTTypeSpecifier) -> Option<Self> {
        match ast_type {
            ASTTypeSpecifier::Void => Some(CaplanType::Void),
            ASTTypeSpecifier::Int => Some(CaplanType::Int),
            _ => None
        }
    }

    pub fn make_pointer(&mut self, linear: bool) {
        let mut t = CaplanType::Void;
        std::mem::swap(self, &mut t);
        *self = if linear {
            CaplanType::LinPtr(Box::new(t))
        } else {
            CaplanType::NonlinPtr(Box::new(t))
        };
    }
}


