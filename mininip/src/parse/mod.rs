//! Provides tools to parse an INI file

use crate::errors::{error_kinds::*, Error};
use std::iter::Fuse;

/// Reads a string formatted by [`dump_str`](../dump/fn.dump_str.html "dump::dump_str") and unescapes the escaped characters
///
/// # Return value
/// `Ok(string)` with `string` as the result once parsed
///
/// `Err(err)` In case of error with `err` as the error code
///
/// # Encoding issues
/// Only allows ASCII because Unicode or other encodings musn't appear in an INI file (except in comments but this function is not intended to parse whole files)
///
/// # Examples
/// ```
/// use mininip::parse::parse_str;
///
/// assert!(parse_str("Bad because ends with a ;").is_err());
/// assert_eq!(parse_str(r"abc\=123\; \x00263a").unwrap(), "abc=123; \u{263a}");
/// ```
pub fn parse_str(content: &str) -> Result<String, Error> {
    // new will never be wider than content

    let mut new = String::with_capacity(content.len());

    static FORBIDDEN: [char; 13] = [
        '\x07', '\x08', '\t', '\r', '\n', '\0', '\\', '\'', '\"', ';', ':', '=', '#',
    ];

    // `next` is the index (as bytes) of the next escape sequence in content
    let mut next = 0;
    for i in TokenIterator::from(content.chars()) {
        let escape = match i {
            Token::Char(c) => {
                let n = next;
                next += 1;

                if FORBIDDEN.contains(&c)
                /*|| !c.is_ascii() */
                {
                    let escape = crate::dump::dump_str(&format!("{}", c));
                    let err = Error::from(ExpectedEscape::new(String::from(content), n, escape));
                    return Err(err);
                }

                new.push(c);
                continue;
            }
            Token::Escape(s) => s,
        };

        next += escape.len();

        match escape.as_str() {
            "\\a" => new.push('\x07'),
            "\\b" => new.push('\x08'),
            "\\t" => new.push('\t'),
            "\\r" => new.push('\r'),
            "\\n" => new.push('\n'),
            "\\0" => new.push('\0'),
            r"\\" => new.push('\\'),
            "\\'" => new.push('\''),
            "\\\"" => new.push('\"'),
            "\\;" => new.push(';'),
            "\\:" => new.push(':'),
            "\\=" => new.push('='),
            "\\#" => new.push('#'),

            _ if escape.len() == 8 => {
                debug_assert!(escape.starts_with("\\x"));

                let values = &escape[2..];
                let code = match u32::from_str_radix(values, 16) {
                    Ok(val) => val,
                    Err(_) => {
                        return Err(Error::from(InvalidEscape::new(
                            String::from(content),
                            escape,
                        )))
                    }
                };
                let character = match std::char::from_u32(code) {
                    Some(val) => val,
                    None => {
                        return Err(Error::from(InvalidEscape::new(
                            String::from(content),
                            escape,
                        )))
                    }
                };
                new.push(character);
            }

            _ => {
                return Err(Error::from(InvalidEscape::new(
                    String::from(content),
                    escape,
                )))
            }
        }
    }

    Ok(new)
}

/// A token which is either a single character or an escape sequence starting with `\`
#[derive(PartialEq, Debug)]
enum Token {
    Char(char),
    Escape(String),
}

/// An iterator over the characters of an INI file
///
/// Yields `Token`s which can be either a character or an escape sequence
///
/// # Safety
/// These characters are NOT TRUSTED, for example, you may receive a `\é` sequence wich is illegal in INI
///
/// If an escape sequence is left unfinished, it is returned as is in a `Token::Escape` object, even though it is invalid
struct TokenIterator<T> {
    iterator: Fuse<T>,
}

impl<T: Iterator> From<T> for TokenIterator<T> {
    fn from(iterator: T) -> TokenIterator<T> {
        TokenIterator {
            iterator: iterator.fuse(),
        }
    }
}

impl<T: Iterator<Item = char>> Iterator for TokenIterator<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut escape_seq = String::with_capacity(8);

        loop {
            let i = match self.iterator.next() {
                Some(val) => val,

                // When the iterator returns `None`, we return the escape sequence if unfinished or `None` if the text was not escaped
                None if escape_seq.is_empty() => return None,
                None => return Some(Token::Escape(escape_seq)),
            };

            if !escape_seq.is_empty() {
                escape_seq.push(i);
            } else if i == '\\' {
                escape_seq.push(i);
                continue;
            } else {
                return Some(Token::Char(i));
            }

            if escape_seq.starts_with(r"\x") && escape_seq.len() < 8 {
                continue;
            }

            return Some(Token::Escape(escape_seq));
        }
    }
}

/// Finds the first non-escaped occurence of `pattern` in `string`
/// . Currently only accepts `char`s
///
/// # Return value
/// `Some(index)` with `index` as the index of the first occurence of `pattern`
///
/// `None` if `pattern` could not be found as a non-escaped form
pub fn find_unescaped(string: &str, pattern: char) -> Option<usize> {
    // possible values of `escape`
    // -1   : the last character parsed is a '\\'
    // 0    : this character must be read because it's unescaped
    // 1..6 : this character must be ignored because it belongs to an escape sequence
    let mut escape = 0;
    for (n, i) in string.char_indices() {
        if escape == -1 {
            escape = if i == 'x' { 6 } else { 0 };
        } else if escape > 0 {
            escape -= 1;
        }
        // Since here, escape = 0 so the character must be parsed
        else if i == '\\' {
            escape = -1;
        } else if i == pattern {
            return Some(n);
        }
    }

    None
}

mod parser;
pub use parser::*;

#[cfg(test)]
mod tests;
