/* Adapted from: https://github.com/TheAlgorithms/Rust */

/*
    A Rust implementation of a base64 encoder and decoder.
    Written from scratch.
*/

include!("../nstd/lib.rs");

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

fn encode_single(byte: u8) -> u8 {
    // Symbolic
    CHARSET[byte as usize]

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
    /*
    let byte = byte as i16;
    (((byte <= 25) as i16) * (byte + b'A' as i16)
        + (((26 <= byte) & (byte < 52)) as i16) * (byte + b'a' as i16 - 26)
        + (((52 <= byte) & (byte < 62)) as i16) * (byte + b'0' as i16 - 52)
        + ((byte == 62) as i16) * (b'+' as i16)
        + ((byte == 63) as i16) * (b'/' as i16)) as u8
    */
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

#[allow(unused)]
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
