use std::cell::RefCell;
use std::rc::Rc;
use std::str::Chars;

pub type GCed<T> = Rc<RefCell<T>>;

pub fn new_gced<T>(data: T) -> GCed<T> {
    Rc::new(RefCell::new(data))
}

pub fn align_up_to(v: usize, align_bits: usize) -> usize {
    ((v >> align_bits) + if (v & ((1 << align_bits) - 1)) == 0 { 0 } else { 1 }) << align_bits
}

fn chars_unescape(chars: &mut Chars) -> Option<char> {
    macro_rules! single_char_unescape {
        ($v:expr) => {
            if matches!(chars.next(), None) {
                Some($v)
            } else {
                None
            }
        };
    }
    chars.next().and_then(
        |escape_c| {
            let res = match escape_c {
                'a' => single_char_unescape!('\x07'),
                'b' => single_char_unescape!('\x08'),
                'e' => single_char_unescape!('\x1b'),
                'f' => single_char_unescape!('\x0c'),
                'n' => {
                    if chars.as_str().is_empty() {
                        Some('\x0a')                        
                    } else {
                        u8::from_str_radix(chars.as_str(), 8).ok().map(|c| c as char)
                    }
                }
                'r' => single_char_unescape!('\x0d'),
                't' => single_char_unescape!('\x09'),
                'v' => single_char_unescape!('\x0b'),
                '\\' => single_char_unescape!('\x5c'),
                '\'' => single_char_unescape!('\x27'),
                '\"' => single_char_unescape!('\x22'),
                '?' => single_char_unescape!('\x3f'),
                'x' =>
                    u8::from_str_radix(chars.as_str(), 16).ok().map(|c| c as char),
                _ => None
            };
            res
        }
    )
}

pub fn char_literal_to_ascii(char_literal: &str) -> Option<char> {
    if !char_literal.is_ascii() {
        return None;
    }

    let mut chars = char_literal.chars();
    chars.next().and_then(
        |first_c| {
            if first_c == '\\' {
                chars_unescape(&mut chars)
            } else if matches!(chars.next(), None) {
                Some(first_c)
            } else {
                None
            }
        }
    )
}
