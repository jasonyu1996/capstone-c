use std::cell::RefCell;
use std::rc::Rc;

pub type GCed<T> = Rc<RefCell<T>>;

pub fn new_gced<T>(data: T) -> GCed<T> {
    Rc::new(RefCell::new(data))
}

pub fn align_up_to(v: usize, align_bits: usize) -> usize {
    (v >> align_bits) + if (v & ((1 << align_bits) - 1)) == 0 { 0 } else { 1 } << align_bits
}
