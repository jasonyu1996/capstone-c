use std::collections::HashMap;
use lang_c::ast::{TypeSpecifier as ASTTypeSpecifier, DerivedDeclarator, ArraySize, Expression, Constant};
use crate::{utils::GCed, lang::CaplanGlobalContext, target_conf::CaplanTargetConf};

pub type CaplanOffsetIter<'a> = dyn Iterator<Item = usize> + 'a;

#[derive(Debug, Clone)]
pub struct CaplanStructField {
    pub ty: CaplanType,
    pub offset: usize // offset in parent
}

#[derive(Debug, Clone)]
pub struct CaplanStruct {
    pub size: usize, // number of bytes
    pub fields: Vec<(String, CaplanStructField)>,
    pub fields_idx: HashMap<String, usize> // field name to index
}

pub struct CaplanStructOffsetIter<'ty> {
    target_conf: &'ty CaplanTargetConf,
    field_iter: std::slice::Iter<'ty, (String, CaplanStructField)>,
    inner_iter_op: Option<Box<CaplanOffsetIter<'ty>>>,
    base_offset: usize
}


impl<'ty> Iterator for CaplanStructOffsetIter<'ty> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(inner_iter) = &mut self.inner_iter_op {
                if let Some(inner_offset) = inner_iter.next() {
                    break Some(inner_offset);
                }
            }
            if let Some(field) = self.field_iter.next() {
                self.inner_iter_op = Some(field.1.ty.offset_iter(self.target_conf))
            } else {
                break None;
            }
        }
    }
}

pub struct CaplanStructRefOffsetIter<'ty> {
    target_conf: &'ty CaplanTargetConf,
    field_iter: Vec<(String, CaplanStructField)>,
    inner_iter_op: Option<Box<CaplanOffsetIter<'ty>>>,
    base_offset: usize
}

impl<'ty> Iterator for CaplanStructRefOffsetIter<'ty> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(inner_iter) = &mut self.inner_iter_op {
                if let Some(inner_offset) = inner_iter.next() {
                    break Some(inner_offset);
                }
            }
            if let Some(field) = self.field_iter.next() {
                self.inner_iter_op = Some(field.1.ty.offset_iter(self.target_conf)) // field got to be owned by someone
            } else {
                break None;
            }
        }
    }
}



pub struct CaplanArrayOffsetIter<'ty> {
    target_conf: &'ty CaplanTargetConf,
    elem_ty: &'ty CaplanType,
    inner_iter: Box<CaplanOffsetIter<'ty>>,
    n: usize,
    base_offset: usize
}

impl<'ty> Iterator for CaplanArrayOffsetIter<'ty> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner_iter.next().or_else(
            || {
                if self.n == 0 {
                    None
                } else {
                    self.n -= 1;
                    self.base_offset += self.elem_ty.size(self.target_conf);
                    self.inner_iter = self.elem_ty.offset_iter(self.target_conf);
                    self.inner_iter.next()
                }
            }
        ).map(|offset| offset + self.base_offset)
    }
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
            fields: Vec::new(),
            fields_idx: HashMap::new()
        }
    }

    pub fn add_field(&mut self, field_name: &str, ty: CaplanType, target_conf: &CaplanTargetConf) {
        // TODO: consider alignment
        let offset = self.size;
        self.size += ty.size(target_conf);
        let field_name_str = String::from(field_name);
        self.fields.push((field_name_str.clone(), CaplanStructField {
            ty: ty,
            offset: offset
        }));
        self.fields_idx.insert(field_name_str, self.fields.len() - 1);
    }

    // returns (offset, type) if found
    pub fn find_field<'a>(&'a self, field_name: &str) -> Option<&'a CaplanStructField> {
        self.fields_idx.get(field_name).map(|&idx| &self.fields[idx].1)
    }

    pub fn offset_iter<'a>(&'a self, target_conf: &'a CaplanTargetConf) -> CaplanStructOffsetIter<'a> {
        CaplanStructOffsetIter {
            target_conf: target_conf,
            field_iter: self.fields.iter(),
            inner_iter_op: None,
            base_offset: 0
        }
    }

    pub fn offset_iter_ref<'a, 'b>(&'b self, target_conf: &'a CaplanTargetConf) -> CaplanStructRefOffsetIter<'a> {
        CaplanStructRefOffsetIter {
            target_conf: target_conf,
            field_iter: self.fields.clone(),
            inner_iter_op: None,
            base_offset: 0
        }
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
            CaplanType::Array(elem_t, elem_n) => elem_t.size(target_conf) * elem_n,
            CaplanType::LinPtr(_) => 16,
            CaplanType::NonlinPtr(_) => 16,
            CaplanType::RawPtr(_) => 8,
            CaplanType::Struct(s) => s.size,
            CaplanType::StructRef(s_ref) => s_ref.borrow().size
        }
    }

    pub fn offset_iter<'a>(&'a self, target_conf: &'a CaplanTargetConf) -> Box<CaplanOffsetIter<'a>> {
        match self {
            CaplanType::Array(elem_t, elem_n) => Box::new(
                CaplanArrayOffsetIter {
                    target_conf: target_conf,
                    base_offset: 0,
                    elem_ty: &*elem_t,
                    n: elem_n - 1,
                    inner_iter: elem_t.offset_iter(target_conf)
                }
            ),
            CaplanType::Struct(s) => Box::new(s.offset_iter(target_conf)),
            CaplanType::StructRef(s_ref) => Box::new(s_ref.borrow().offset_iter_ref(target_conf)),
            _ => Box::new([0].into_iter())
        }
    }
}


