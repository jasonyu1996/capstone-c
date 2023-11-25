use std::collections::HashMap;
use lang_c::ast::{TypeSpecifier as ASTTypeSpecifier, DerivedDeclarator, ArraySize, Expression, Constant};
use crate::{utils::GCed, lang::CaplanGlobalContext};

#[derive(Debug, Clone)]
pub struct CaplanStructField {
    pub ty: CaplanType,
    pub offset: usize // offset in parent
}

#[derive(Debug, Clone)]
pub struct CaplanStruct {
    pub size: usize, // number of bytes
    pub fields: HashMap<String, CaplanStructField>
}

#[derive(Debug, Clone)]
pub enum CaplanType {
    Void,
    Int,
    Dom,
    Array(Box<CaplanType>, usize), // element type and length
    LinPtr(Box<CaplanType>),
    NonlinPtr(Box<CaplanType>),
    RawPtr(Box<CaplanType>),
    Struct(Box<CaplanStruct>), // TODO: this is not supported for now
    StructRef(GCed<CaplanStruct>) // referencing a struct def elsewhere
}

impl CaplanStruct {
    pub fn new() -> Self {
        Self {
            size: 0,
            fields: HashMap::new()
        }
    }

    pub fn add_field(&mut self, field_name: &str, ty: CaplanType) {
        let offset = self.size;
        self.size += ty.size();
        self.fields.insert(String::from(field_name), CaplanStructField {
            ty: ty,
            offset: offset
        });
    }

    // returns (offset, type) if found
    pub fn find_field<'a>(&'a self, field_name: &str) -> Option<&'a CaplanStructField> {
        self.fields.get(field_name)
    }
}

impl CaplanType {
    pub fn from_ast_type(ast_type: &ASTTypeSpecifier, globals: &CaplanGlobalContext) -> Option<Self> {
        match ast_type {
            ASTTypeSpecifier::Void => Some(CaplanType::Void),
            ASTTypeSpecifier::Char => Some(CaplanType::Int),
            ASTTypeSpecifier::Short => Some(CaplanType::Int),
            ASTTypeSpecifier::Int => Some(CaplanType::Int),
            ASTTypeSpecifier::Long => Some(CaplanType::Int),
            ASTTypeSpecifier::Signed => Some(CaplanType::Int),
            ASTTypeSpecifier::Unsigned => Some(CaplanType::Int),
            ASTTypeSpecifier::Bool => Some(CaplanType::Int),
            ASTTypeSpecifier::Struct(struct_type) => {
                assert!(struct_type.node.declarations.is_none());
                let struct_name = &struct_type.node.identifier.as_ref().unwrap().node.name;
                let struct_ref = globals.struct_defs.get(struct_name).unwrap().clone();
            Some(CaplanType::StructRef(struct_ref))
            }
            _ => None
        }
    }

    pub fn decorate_from_ast(&mut self, derived_declarator: &DerivedDeclarator) {
        match derived_declarator {
            DerivedDeclarator::Pointer(_) => self.make_pointer(false),
            DerivedDeclarator::Array(arr_declarator) => {
                let length_opt = match &arr_declarator.node.size {
                    ArraySize::StaticExpression(expr) => {
                        match &expr.node {
                            Expression::Constant(constant) => {
                                match &constant.node {
                                    Constant::Integer(integer) => {
                                        usize::from_str_radix(&*integer.number, 10).ok()
                                    }
                                    _ => None
                                }
                            }
                            _ => None
                        }
                    }
                    _ => None
                };
                if let Some(length) = length_opt {
                    self.make_array(length);
                } else {
                    panic!("Invalid array size");
                }
            }
            _ => ()
        }
    }

    pub fn make_pointer(&mut self, linear: bool) {
        let mut t = CaplanType::Void;
        std::mem::swap(self, &mut t);
        *self = if linear {
            CaplanType::LinPtr(Box::new(t))
        } else {
            // CaplanType::NonlinPtr(Box::new(t))
            CaplanType::RawPtr(Box::new(t))
        };
    }

    pub fn make_array(&mut self, count: usize) {
        let mut t = CaplanType::Void;
        std::mem::swap(self, &mut t);
        *self = CaplanType::Array(Box::new(t), count);
    }

    pub fn size(&self) -> usize {
        match self {
            CaplanType::Void => 8,
            CaplanType::Int => 8,
            CaplanType::Dom => 16,
            CaplanType::Array(elem_t, elem_n) => elem_t.size() * elem_n,
            CaplanType::LinPtr(_) => 16,
            CaplanType::NonlinPtr(_) => 16,
            CaplanType::RawPtr(_) => 8,
            CaplanType::Struct(s) => s.size,
            CaplanType::StructRef(s_ref) => s_ref.borrow().size
        }
    }
}


