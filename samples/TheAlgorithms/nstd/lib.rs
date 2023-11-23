// Non-STD Leaf-compatible Library

fn ns_panic() -> ! {
    std::panic::panic_any("NS Panic")
}

macro_rules! ns_panic {
    ($($arg:tt)*) => {{ ns_panic() }};
}

macro_rules! ns_assert {
    ($cond:expr) => {
        if !$cond {
            ns_panic!("assertion failed: {}", stringify!($cond));
        }
    };
}

macro_rules! ns_assert_eq {
    ($left:expr, $right:expr) => {
        if $left != $right {
            ns_panic!(
                "assertion failed: `(left == right)`\n  left: `{:?}`,\n right: `{:?}`",
                $left,
                $right
            );
        }
    };
}

// Non-STD Result
enum NSResult<T, E> {
    Ok(T),
    Err(E),
}
use std::ops::Index;

use NSResult::*;

impl<T, E> NSResult<T, E> {
    fn unwrap(self) -> T {
        match self {
            Ok(value) => value,
            Err(_) => ns_panic!("Called `unwrap` on an `NSResult::Err` value"),
        }
    }
}

// Non-STD Option
enum NSOption<T> {
    Some(T),
    None,
}
use NSOption::*;

impl<T> NSOption<T> {
    fn unwrap(self) -> T {
        match self {
            Some(value) => value,
            None => ns_panic!("Called `unwrap` on an `NSOption::None` value"),
        }
    }
}

trait NSIterator {
    type Item;

    fn next(&mut self) -> NSOption<Self::Item>;
}

trait NSIntoIterator {
    type Item;
    type IntoIter: NSIterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter;
}

pub struct NSRange<Idx> {
    /// The lower bound of the range (inclusive).
    pub start: Idx,
    /// The upper bound of the range (exclusive).
    pub end: Idx,
}

impl<Idx> NSRange<Idx> {
    pub fn new(start: Idx, end: Idx) -> Self {
        Self { start, end }
    }
}

struct NSRangeIterator<Idx> {
    range: NSRange<Idx>,
    index: Idx,
}

impl<Idx> NSIterator for NSRangeIterator<Idx>
where
    Idx: std::ops::AddAssign<usize> + std::cmp::PartialOrd + Copy,
{
    type Item = Idx;

    fn next(&mut self) -> NSOption<Self::Item> {
        if self.index < self.range.end {
            let index = self.index;
            self.index += 1;
            Some(index)
        } else {
            None
        }
    }
}

impl<Idx> NSIntoIterator for NSRange<Idx>
where
    Idx: std::ops::AddAssign<usize> + std::cmp::PartialOrd + Clone + Copy,
{
    type Item = Idx;
    type IntoIter = NSRangeIterator<Idx>;

    fn into_iter(self) -> Self::IntoIter {
        let index = (&self.start).clone();
        NSRangeIterator { range: self, index }
    }
}

const STRING_MAX_CAP: usize = 1000;
struct NSString {
    chars: [char; STRING_MAX_CAP],
    len: usize,
}

impl NSString {
    fn new() -> Self {
        Self {
            chars: ['\0'; STRING_MAX_CAP],
            len: 0,
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn push(&mut self, c: char) {
        self.chars[self.len] = c;
        self.len += 1;
    }
}

impl PartialEq<NSString> for NSString {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }

        let mut i = 0;
        while i < self.len() {
            if self.chars[i] != other.chars[i] {
                return false;
            }
            i += 1;
        }

        true
    }
}

struct NSByteString {
    bytes: [u8; STRING_MAX_CAP],
    len: usize,
}

struct NSByteStringIter<'a> {
    byte_str: &'a NSByteString,
    index: usize,
}

impl NSIterator for NSByteStringIter<'_> {
    type Item = u8;

    fn next(&mut self) -> NSOption<Self::Item> {
        if self.index < self.byte_str.len {
            let byte = self.byte_str.bytes[self.index];
            self.index += 1;
            Some(byte)
        } else {
            None
        }
    }
}

impl NSByteString {
    fn new() -> Self {
        Self {
            bytes: [0; STRING_MAX_CAP],
            len: 0,
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn push(&mut self, byte: u8) {
        self.bytes[self.len] = byte;
        self.len += 1;
    }

    fn iter(&self) -> NSByteStringIter {
        NSByteStringIter {
            byte_str: self,
            index: 0,
        }
    }
}

impl Index<usize> for NSByteString {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.bytes[index]
    }
}

impl PartialEq<NSByteString> for NSByteString {
    // Regular
    /*
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }

        let mut i = 0;
        while i < self.len {
            if self.bytes[i] != other.bytes[i] {
                return false;
            }
            i += 1;
        }

        true
    }
    */

    // Non-short-circuiting
    fn eq(&self, other: &Self) -> bool {
        let mut result = true;

        result &= self.len == other.len;

        let mut i = 0;
        while i < self.len {
            result &= self.bytes[i] == other.bytes[i];
            i += 1;
        }

        result
    }

    fn ne(&self, other: &Self) -> bool {
        // We have to add this to not rely on the uninstrumented default implementation.
        !self.eq(other)
    }
}

impl From<&[u8]> for NSByteString {
    fn from(bytes: &[u8]) -> Self {
        let mut byte_str = Self::new();
        let mut i = 0;
        while i < bytes.len() {
            byte_str.push(bytes[i]);
            i += 1;
        }
        byte_str
    }
}

impl<const N: usize> From<[u8; N]> for NSByteString {
    fn from(bytes: [u8; N]) -> Self {
        Self::from(&bytes)
    }
}

impl<const N: usize> From<&[u8; N]> for NSByteString {
    fn from(bytes: &[u8; N]) -> Self {
        Self::from(bytes as &[u8])
    }
}

impl<'a> From<&'a NSString> for NSByteString {
    fn from(string: &'a NSString) -> Self {
        let mut byte_str = Self::new();
        let mut i = 0;
        while i < string.len() {
            byte_str.push(string.chars[i] as u8);
            i += 1;
        }
        byte_str
    }
}
