use std::collections::HashMap;
use lang_c::ast::{TypeSpecifier as ASTTypeSpecifier, DerivedDeclarator, ArraySize, Expression, Constant};
use crate::{utils::GCed, lang::CaplanGlobalContext, target_conf::{CaplanTargetConf, self}, dag::IRDAGNodeVType};

#[derive(Debug, Clone)]
pub struct CaplanStructField {
    pub ty: CaplanType,
    pub offset: usize // offset in parent
}

#[derive(Debug, Clone)]
pub struct CaplanStruct {
    pub size: usize, // number of bytes
    pub alignment: usize,
    pub fields: Vec<(String, CaplanStructField)>,
    pub fields_idx: HashMap<String, usize>
}

#[derive(Clone)]
pub enum CaplanType {
    Void,
    Int,
    Dom,
    DomRet,
    DomAsync, // asynchronous sealed-return cap
    Rev(Box<CaplanType>), // revocation capability
    Array(Box<CaplanType>, usize), // element type and length
    LinPtr(Box<CaplanType>),
    NonlinPtr(Box<CaplanType>),
    RawPtr(Box<CaplanType>),
    Struct(Box<CaplanStruct>), // TODO: this is not supported for now
    StructRef(GCed<CaplanStruct>) // referencing a struct def elsewhere
}

impl std::fmt::Debug for CaplanType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CaplanType::Void => write!(f, "Void"),
            CaplanType::Int => write!(f, "Int"),
            CaplanType::Dom => write!(f, "Dom"),
            CaplanType::DomRet => write!(f, "DomRet"),
            CaplanType::DomAsync => write!(f, "DomAsync"),
            CaplanType::Rev(inner_type) => f.debug_tuple("Rev").field(inner_type).finish(),
            CaplanType::Array(inner_type, n) => 
                f.debug_tuple("Array").field(inner_type).field(n).finish(),
            CaplanType::LinPtr(inner_type) =>
                f.debug_tuple("LinPtr").field(inner_type).finish(),
            CaplanType::NonlinPtr(inner_type) =>
                f.debug_tuple("NonlinPtr").field(inner_type).finish(),
            CaplanType::RawPtr(inner_type) =>
                f.debug_tuple("RawPtr").field(inner_type).finish(),
            CaplanType::Struct(_) => write!(f, "Struct"),
            CaplanType::StructRef(_) => write!(f, "Struct")
        }
    }
}

impl CaplanStruct {
    pub fn new() -> Self {
        Self {
            size: 0,
            alignment: 8,
            fields: Vec::new(),
            fields_idx: HashMap::new()
        }
    }

    pub fn add_field(&mut self, field_name: &str, ty: CaplanType, target_conf: &CaplanTargetConf) {
        // TODO: consider alignment here
        let offset = self.size;
        let field_name_str = String::from(field_name);
        self.size += ty.size(target_conf);
        if ty.alignment(target_conf) == 16 {
            self.alignment = 16;
        }
        self.fields.push((field_name_str.clone(), CaplanStructField {
            ty: ty,
            offset: offset
        }));
        self.fields_idx.insert(field_name_str, self.fields.len() - 1);
    }

    // returns (offset, type) if found
    pub fn find_field<'a>(&'a self, field_name: &str) -> Option<&'a CaplanStructField> {
        self.fields_idx.get(field_name).map(|idx| &self.fields[*idx].1)
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

    pub fn decorate_from_ast(&mut self, derived_declarator: &DerivedDeclarator, target_conf: &CaplanTargetConf) {
        match derived_declarator {
            DerivedDeclarator::Pointer(_) => self.make_pointer(target_conf),
            DerivedDeclarator::Array(arr_declarator) => {
                let length_opt = match &arr_declarator.node.size {
                    ArraySize::VariableExpression(expr) => {
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
                    eprintln!("Make array of length {}", length);
                    self.make_array(length);
                } else {
                    panic!("Invalid array size");
                }
            }
            _ => ()
        }
    }

    pub fn is_linear(&self) -> bool {
        match self {
            CaplanType::LinPtr(_) | CaplanType::Rev(_) | CaplanType::Dom | CaplanType::DomRet => true,
            _ => false
        }
    }

    pub fn make_pointer(&mut self, target_conf: &CaplanTargetConf) {
        target_conf.make_pointer(self);
    }

    pub fn make_linear_pointer(&mut self) {
        let t = std::mem::replace(self, CaplanType::Void);
        *self = CaplanType::LinPtr(Box::new(t));
    }

    pub fn make_array(&mut self, count: usize) {
        let t = std::mem::replace(self, CaplanType::Void);
        *self = CaplanType::Array(Box::new(t), count);
    }

    pub fn size(&self, target_conf: &CaplanTargetConf) -> usize {
        match self {
            CaplanType::Void => 8,
            CaplanType::Int => 8,
            CaplanType::Dom => 16,
            CaplanType::DomRet => 16,
            CaplanType::DomAsync => 16,
            CaplanType::Rev(_) => 16,
            CaplanType::Array(elem_t, elem_n) => elem_t.size(target_conf) * elem_n,
            CaplanType::LinPtr(_) => 16,
            CaplanType::NonlinPtr(_) => 16,
            CaplanType::RawPtr(_) => 8,
            CaplanType::Struct(s) => s.size,
            CaplanType::StructRef(s_ref) => s_ref.borrow().size
        }
    }

    pub fn alignment(&self, target_conf: &CaplanTargetConf) -> usize {
        match self {
            CaplanType::Void => 8,
            CaplanType::Int => 8,
            CaplanType::Dom => 16,
            CaplanType::DomRet => 16,
            CaplanType::DomAsync => 16,
            CaplanType::Rev(_) => 16,
            CaplanType::Array(elem_t, elem_n) => elem_t.alignment(target_conf), // TODO: padding possibly needed
            CaplanType::LinPtr(_) => 16,
            CaplanType::NonlinPtr(_) => 16,
            CaplanType::RawPtr(_) => 8,
            CaplanType::Struct(s) => s.alignment,
            CaplanType::StructRef(s_ref) => s_ref.borrow().alignment
        }
    }

    pub fn visit_offset<V>(&self, visitor: &mut V, base_offset: usize, target_conf: &CaplanTargetConf) where V: FnMut(usize, CaplanType) {
        match self {
            CaplanType::Array(elem_t, elem_n) => {
                let mut offset = base_offset;
                for _ in 0..*elem_n {
                    elem_t.visit_offset(visitor, offset, target_conf);
                    offset += elem_t.size(target_conf);
                }
            }
            CaplanType::Struct(s) => {
                for (_, field) in s.fields.iter() {
                    field.ty.visit_offset(visitor, base_offset + field.offset, target_conf);
                }
            }
            CaplanType::StructRef(s_ref) => {
                for (_, field) in s_ref.borrow().fields.iter() {
                    field.ty.visit_offset(visitor, base_offset + field.offset, target_conf);
                }
            }
            _ => visitor(base_offset, self.clone())
        }
    }

    pub fn collect_offsets(&self, target_conf: &CaplanTargetConf) -> Vec<(usize, CaplanType)> {
        let mut res : Vec<(usize, CaplanType)> = Vec::new();
        self.visit_offset(&mut |offset, ty| {
            res.push((offset, ty))
        }, 0, target_conf);
        res
    }
}


// attributes in GNU extension

// attributes for types. We see them as modifiers of types
pub struct TypeAttribute<'h> {
    pub name: &'h str,
    // this is intended to be called after the other parts of the type specifier has been processed
    pub type_modifier: &'h dyn Fn(&mut CaplanType) -> bool 
}

macro_rules! builtin_type_attr {
    ($name:expr, $ty:expr) => {
        TypeAttribute {
            name: $name,
            type_modifier: &|ty: &mut CaplanType| {
                *ty = $ty;
                true
            }
        }
    }
}

pub const TYPE_ATTRIBUTES : &'static [TypeAttribute<'static>] = &[
    TypeAttribute {
        name: "linear",
        type_modifier: &|ty: &mut CaplanType| {
            let orig_type = std::mem::replace(ty, CaplanType::Void);
            match orig_type {
                CaplanType::NonlinPtr(inner_type) | CaplanType::LinPtr(inner_type) => {
                    *ty = CaplanType::LinPtr(inner_type);
                    true
                }
                _ => false
            }
        }
    },
    TypeAttribute {
        name: "rev",
        type_modifier: &|ty: &mut CaplanType| {
            let orig_type = std::mem::replace(ty, CaplanType::Void);
            match orig_type {
                CaplanType::NonlinPtr(inner_type) | CaplanType::LinPtr(inner_type) => {
                    *ty = CaplanType::Rev(inner_type);
                    true
                }
                _ => false
            }
        }
    },
    builtin_type_attr!("dom", CaplanType::Dom),
    builtin_type_attr!("domret", CaplanType::DomRet),
    builtin_type_attr!("domasync", CaplanType::DomAsync)
];

pub fn try_modify_type_with_attr(ty: &mut CaplanType, attr_name: &str) -> bool {
    for attr in TYPE_ATTRIBUTES.iter() {
        if attr.name == attr_name {
            return (attr.type_modifier)(ty);
        }
    }
    false
}

#[derive(Clone, Copy, Debug)]
pub enum IntrinsicFunction {
    Mrev,
    Revoke,
    Seal,
    Delin,
    Tighten,
    DomCall,
    DomReturn
}

impl IntrinsicFunction {
    pub fn get_return_type(&self, arg_types: &[&IRDAGNodeVType], _: &CaplanTargetConf) -> Option<IRDAGNodeVType> {
        match self {
            IntrinsicFunction::Mrev => {
                if arg_types.len() < 1 {
                    None
                } else if let IRDAGNodeVType::LinPtr(inner) = arg_types[0] {
                    Some(IRDAGNodeVType::Rev(inner.clone()))
                } else {
                    None
                }
            }
            IntrinsicFunction::Revoke => {
                if arg_types.len() < 1 {
                    None
                } else if let IRDAGNodeVType::Rev(inner) = arg_types[0] {
                    Some(IRDAGNodeVType::LinPtr(inner.clone())) // TODO: what to do with uninitialised capabilities
                } else {
                    None
                }
                
            }
            IntrinsicFunction::Seal => {
                if arg_types.len() < 1 {
                    None
                } else if let IRDAGNodeVType::LinPtr(_) = arg_types[0] {
                    Some(IRDAGNodeVType::Dom)
                } else {
                    None
                }
            }
            IntrinsicFunction::Delin => {
                if arg_types.len() < 1 {
                    None
                } else if let IRDAGNodeVType::LinPtr(inner) = arg_types[0] {
                    Some(IRDAGNodeVType::NonlinPtr(inner.clone()))
                } else {
                    None
                }
            }
            IntrinsicFunction::Tighten => {
                if arg_types.len() < 2 {
                    None
                } else {
                    match arg_types[0] {
                        IRDAGNodeVType::LinPtr(_) | IRDAGNodeVType::NonlinPtr(_) => Some(arg_types[0].clone()),
                        _ => None
                    }
                }
            }
            IntrinsicFunction::DomCall => {
                if arg_types.len() < 1 {
                    None
                } else if let IRDAGNodeVType::Dom = arg_types[0] {
                    Some(IRDAGNodeVType::Dom)
                } else {
                    None
                }
            }
            IntrinsicFunction::DomReturn => {
                if arg_types.len() < 3 {
                    None
                } else {
                    match (&arg_types[0], &arg_types[1], &arg_types[2]) {
                        // TODO: add dedicated type for sealed-return capability
                        (IRDAGNodeVType::DomRet, IRDAGNodeVType::Int, IRDAGNodeVType::Int) 
                        | (IRDAGNodeVType::DomAsync, IRDAGNodeVType::Int, IRDAGNodeVType::Int) => Some(IRDAGNodeVType::Void),
                        _ => None
                    }
                }
            }
        }
    }

    pub fn get_destructives(&self, arg_types: &[&IRDAGNodeVType]) -> Vec<usize> {
        match self {
            IntrinsicFunction::Mrev => vec![],
            IntrinsicFunction::Revoke => vec![0],
            IntrinsicFunction::Seal => vec![0],
            IntrinsicFunction::Delin => vec![0],
            IntrinsicFunction::Tighten =>
                if arg_types[0].is_linear() {
                    vec![0]
                } else {
                    vec![]
                },
            IntrinsicFunction::DomCall => arg_types.into_iter().enumerate().filter_map(|(idx, ty)| 
                // if ty.is_linear() {
                    Some(idx)).collect(),
                // } else {
                    // None
                // }).collect(),
            IntrinsicFunction::DomReturn => vec![0]
                // if arg_types.get(1).filter(|x| x.is_linear()).is_some() {
                //     vec![0, 1]
                // } else {
                    // ![0]
                // }
        } 
    }

    pub fn is_control_flow(&self) -> bool {
        match self {
            IntrinsicFunction::Mrev | IntrinsicFunction::Revoke
            | IntrinsicFunction::Seal | IntrinsicFunction::Delin 
            | IntrinsicFunction::Tighten => false,
            IntrinsicFunction::DomCall | IntrinsicFunction::DomReturn => true
        }
    }
}

const INTRINSIC_FUNCS : &'static [(&'static str, IntrinsicFunction)] = &[
    ("__mrev", IntrinsicFunction::Mrev),
    ("__revoke", IntrinsicFunction::Revoke),
    ("__seal", IntrinsicFunction::Seal),
    ("__delin", IntrinsicFunction::Delin),
    ("__tighten", IntrinsicFunction::Tighten),
    ("__domcall", IntrinsicFunction::DomCall),
    ("__domreturn", IntrinsicFunction::DomReturn)
];

pub fn lookup_intrinsic(name: &str) -> Option<IntrinsicFunction> {
    INTRINSIC_FUNCS.iter().find_map(
        |(i_name, intrinsic)| {
            if *i_name == name {
                Some(*intrinsic)
            } else {
                None
            }
        }
    )
}
