use std::cell::RefCell;
use std::rc::Rc;

pub type GCed<T> = Rc<RefCell<T>>;

pub fn new_gced<T>(data: T) -> GCed<T> {
    Rc::new(RefCell::new(data))
}
