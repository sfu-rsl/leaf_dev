/* Adapted from: https://github.com/TheAlgorithms/Rust */

/*
    A Rust implementation of a base64 encoder and decoder.
    Written from scratch.
*/

fn main() {
    /* Only the final case may perform useful symbolic execution.
     * the rest can be used to ensure engine's support.
     */
    // tests::pregenerated_random_bytes_encode();
    // tests::pregenerated_random_bytes_decode();
    // tests::encode_decode();
    // tests::decode_encode();
    // tests::encode_symbolic();
    // tests::decode_symbolic();
    tests::encode_decode_symbolic();
}

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

impl PartialEq<&str> for NSString {
    fn eq(&self, other: &&str) -> bool {
        if self.len != other.len() {
            return false;
        }

        let mut i = 0;
        while i < self.len() {
            if self.chars[i] != other.chars().nth(i).unwrap() {
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

// The charset and padding used for en- and decoding.
const CHARSET: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const PADDING: u8 = b'=';

/*
    Combines the two provided bytes into an u16,
    and collects 6 bits from it using an AND mask:

    Example:
    Bytes: X and Y
    (Bits of those bytes will be signified using the names of their byte)
    Offset: 4

    `combined` = 0bXXXXXXXXYYYYYYYY
    AND mask:
    0b1111110000000000 >> offset (4) = 0b0000111111000000
    `combined` with mask applied:
    0b0000XXYYYY000000
    Shift the value right by (16 bit number) - (6 bit mask) - (4 offset) = 6:
    0b0000000000XXYYYY
    And then turn it into an u8:
    0b00XXYYYY (Return value)
*/
fn collect_six_bits(from: (u8, u8), offset: u8) -> u8 {
    let combined: u16 = ((from.0 as u16) << 8_u16) | (from.1 as u16);
    ((combined & (0b1111110000000000u16 >> (offset as u16))) >> (10 - offset as u16)) as u8
}

fn index_of(byte: u8) -> NSOption<usize> {
    let mut i = 0;
    while i < CHARSET.len() {
        if CHARSET[i] == byte {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Equivalent of CHARSET[byte as usize]
fn encode_single(byte: u8) -> u8 {
    // Buggy
    /*
    match byte {
        0..=51 => byte + b'A',
        52..=61 => byte - 52 + b'0',
        62 => b'+',
        63 => b'/',
        _ => ns_panic!("Invalid byte to encode: {}", byte),
    };
    */

    // Branching
    /*
    match byte {
        0..=25 => byte + b'A',
        26..=51 => byte - 26 + b'a',
        52..=61 => byte - 52 + b'0',
        62 => b'+',
        63 => b'/',
        _ => ns_panic!("Invalid byte to encode: {}", byte),
    };
    */

    // Non-branching
    let byte = byte as i16;
    (((byte <= 25) as i16) * (byte + b'A' as i16)
        + (((26 <= byte) & (byte < 52)) as i16) * (byte + b'a' as i16 - 26)
        + (((52 <= byte) & (byte < 62)) as i16) * (byte + b'0' as i16 - 52)
        + ((byte == 62) as i16) * (b'+' as i16)
        + ((byte == 63) as i16) * (b'/' as i16)) as u8
}

fn base64_encode(
    // data: &[u8]
    data: &NSByteString,
) -> NSByteString {
    let mut bits_encoded = 0usize;
    let mut encoded_string = NSByteString::new();
    // Using modulo twice to prevent an underflow, Wolfram|Alpha says this is optimal
    let padding_needed = ((6 - (data.len() * 8) % 6) / 2) % 3;
    loop {
        let lower_byte_index_to_encode = bits_encoded / 8usize; // Integer division
        if lower_byte_index_to_encode == data.len() {
            break;
        }
        let lower_byte_to_encode = data[lower_byte_index_to_encode];
        let upper_byte_to_encode = if (lower_byte_index_to_encode + 1) == data.len() {
            0u8 // Padding
        } else {
            data[lower_byte_index_to_encode + 1]
        };
        let bytes_to_encode = (lower_byte_to_encode, upper_byte_to_encode);
        let offset: u8 = (bits_encoded % 8) as u8;
        encoded_string.push(encode_single(collect_six_bits(bytes_to_encode, offset)));
        bits_encoded += 6;
    }
    let mut i = 0;
    while i < padding_needed {
        encoded_string.push(PADDING);
        i += 1;
    }
    encoded_string
}

/*
    Performs the exact inverse of the above description of `base64_encode`
*/
fn base64_decode(
    // data: &str,
    mut databytes: impl NSIterator<Item = u8>,
) -> NSResult<NSByteString, (&'static str, u8)> {
    let mut collected_bits = 0;
    let mut byte_buffer = 0u16;
    // let mut databytes = data.bytes();
    let mut outputbytes = NSByteString::new();
    'decodeloop: loop {
        while collected_bits < 8 {
            if let Some(nextbyte) = databytes.next() {
                // Finds the first occurence of the latest byte
                if let Some(idx) = index_of(nextbyte) {
                    byte_buffer |= ((idx & 0b00111111) as u16) << (10 - collected_bits);
                    collected_bits += 6;
                } else if nextbyte == (PADDING as u8) {
                    collected_bits -= 2; // Padding only comes at the end so this works
                } else {
                    return Err((
                        "Failed to decode base64: Expected byte from charset, found invalid byte.",
                        nextbyte,
                    ));
                }
            } else {
                break 'decodeloop;
            }
        }
        outputbytes.push(((0b1111111100000000 & byte_buffer) >> 8) as u8);
        byte_buffer &= 0b0000000011111111;
        byte_buffer <<= 8;
        collected_bits -= 8;
    }
    if collected_bits != 0 {
        return Err(("Failed to decode base64: Invalid padding.", collected_bits));
    }
    Ok(outputbytes)
}

mod tests {
    use super::*;

    pub(super) fn pregenerated_random_bytes_encode() {
        macro_rules! test_encode {
            ($left: expr, $right: expr) => {
                ns_assert_eq!(
                    base64_encode(&NSByteString::from($left)),
                    NSByteString::from($right)
                );
            };
        }
        test_encode!(
            b"\xd31\xc9\x87D\xfe\xaa\xb3\xff\xef\x8c\x0eoD",
            b"0zHJh0T+qrP/74wOb0Q="
        );
        test_encode!(
            b"\x9f\x0e8\xbc\xf5\xd0-\xb4.\xd4\xf0?\x8f\xe7\t{.\xff/6\xcbTY!\xae9\x82",
            b"nw44vPXQLbQu1PA/j+cJey7/LzbLVFkhrjmC"
        );
    }

    pub(super) fn pregenerated_random_bytes_decode() {
        macro_rules! test_decode {
            ($left: expr, $right: expr) => {
                ns_assert_eq!(base64_decode($left).unwrap(), $right);
            };
        }
        test_decode!(
            NSByteString::from(b"0zHJh0T+qrP/74wOb0Q=").iter(),
            NSByteString::from(b"\xd31\xc9\x87D\xfe\xaa\xb3\xff\xef\x8c\x0eoD")
        );
        test_decode!(
            NSByteString::from(b"nw44vPXQLbQu1PA/j+cJey7/LzbLVFkhrjmC").iter(),
            NSByteString::from(
                b"\x9f\x0e8\xbc\xf5\xd0-\xb4.\xd4\xf0?\x8f\xe7\t{.\xff/6\xcbTY!\xae9\x82"
            )
        );
    }

    pub(super) fn encode_decode() {
        macro_rules! test_e_d {
            ($text: expr) => {
                ns_assert_eq!(base64_decode(base64_encode(&$text).iter()).unwrap(), $text);
            };
        }
        test_e_d!(NSByteString::from(b"Lorem Ipsum sit dolor amet."));
        test_e_d!(NSByteString::from(b"0123456789"));
    }

    pub(super) fn decode_encode() {
        macro_rules! test_d_e {
            ($data: expr) => {
                ns_assert_eq!(base64_encode(&base64_decode($data.iter()).unwrap()), $data);
            };
        }
        test_d_e!(NSByteString::from(b"TG9uZyBsaXZlIGVhc3RlciBlZ2dzIDop"));
        test_d_e!(NSByteString::from(b"SGFwcHkgSGFja3RvYmVyZmVzdCE="));
    }

    pub(super) fn encode_symbolic() {
        use runtime::annotations::Symbolizable;

        let data = b"\xd31\xc9\x87D\xfe\xaa\xb3\xff\xef\x8c\x0eoD";
        let mut data_sym = NSByteString::new();
        let mut i = 0;
        while i < data.len() {
            data_sym.push(data[i].mark_symbolic());
            i += 1;
        }

        let encoded = base64_encode(&data_sym);
        let expected = NSByteString::from(b"0zHJh0T+qrP/74wOb0Q=");
        ns_assert_eq!(encoded, expected);
    }

    pub(super) fn decode_symbolic() {
        use runtime::annotations::Symbolizable;

        let encoded = b"0zHJh0T+qrP/74wOb0Q=";
        let mut encoded_sym = NSByteString::new();
        let mut i = 0;
        while i < encoded.len() {
            encoded_sym.push(encoded[i].mark_symbolic());
            i += 1;
        }

        let decoded = base64_decode(encoded_sym.iter()).unwrap();
        let expected = NSByteString::from(b"\xd31\xc9\x87D\xfe\xaa\xb3\xff\xef\x8c\x0eoD");
        ns_assert_eq!(decoded, expected);
    }

    pub(super) fn encode_decode_symbolic() {
        use runtime::annotations::Symbolizable;

        let data = b"Lorem Ipsum";
        let mut data_sym = NSByteString::new();
        let mut i = 0;
        while i < data.len() {
            data_sym.push(data[i].mark_symbolic());
            i += 1;
        }

        let encoded = base64_encode(&data_sym);
        let decoded = base64_decode(encoded.iter()).unwrap();
        ns_assert_eq!(decoded, NSByteString::from(data));
    }
}
