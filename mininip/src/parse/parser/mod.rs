//! Contains the definition of [`Parser`](struct.Parser.html "parse::Parser")

use std::collections::HashMap;
use crate::datas::{Identifier, Value};
use crate::errors::{Error, error_kinds::*, ParseFileError};
use std::path::Path;
use std::fs::File;
use std::io::Read;

/// A parser with a local state. Use it by passing it the text to parse line after line
/// 
/// # Notes
/// This parser does not work with a file but with lines passed as below. It allows you to parse an INI data from an iterator, a channel, the network...
/// # Examples
/// ```
/// use mininip::parse::Parser;
/// use mininip::datas::{Identifier, Value};
/// 
/// let mut parser = Parser::new();
/// 
/// parser.parse_line("abc = 123").unwrap();
/// parser.parse_line("; comment. A comment may start at an end of line").unwrap();
/// parser.parse_line("").unwrap(); // empty line
/// parser.parse_line("[section]").unwrap();
/// parser.parse_line("def = '\\;) \\= \\x00263a' ; This is perfectly valid").unwrap();
/// 
/// let data = parser.data();
/// 
/// let section = None;
/// let name = String::from("abc");
/// let abc = Identifier::new(section, name);
/// 
/// let value = Value::Int(123);
/// assert_eq!(data[&abc], value);
/// 
/// let section = Some(String::from("section"));
/// let name = String::from("def");
/// let def = Identifier::new(section, name);
/// 
/// let value = Value::Str(String::from(";) = \u{263a}"));
/// assert_eq!(data[&def], value);
/// ```
#[derive(Debug, Clone)]
pub struct Parser {
    variables: HashMap<Identifier, Value>,
    cur_section: Option<String>,
}

impl Parser {
    /// Creates a new `Parser`, which didn't parsed any line
    pub fn new() -> Parser {
        Parser {
            variables: HashMap::new(),
            cur_section: None,
        }
    }

    /// Consumes the parser and returns its data which is an `HashMap<Identifier, Value>` linking an identifier to its value
    pub fn data(self) -> HashMap<Identifier, Value> {
        self.variables
    }

    /// Parses a line
    /// 
    /// # Parameters
    /// `line` the line to parse
    /// 
    /// # Return value
    /// `Ok(())` in case of success
    /// 
    /// `Err(error)` in case of error with `error` as the error code (see [`Error`](../errors/enum.Error.html "errors::Error"))
    /// 
    /// # Examples
    /// ```rust
    /// use mininip::parse::Parser;
    /// use mininip::errors::{Error, error_kinds};
    /// use mininip::datas::{Identifier, Value};
    /// 
    /// let mut parser = Parser::new();
    /// 
    /// let good_line = "greeting = Hello \\x00263a";
    /// parser.parse_line(good_line)
    ///     .expect("This line is valid");
    /// 
    /// let bad_line = "how to greet? = Hello \\x00263a";
    /// match parser.parse_line(bad_line) {
    ///     Ok(())                             => panic!("This line is invalid and should not be accepted"),
    ///     Err(Error::InvalidIdentifier(err)) => assert_eq!(format!("{}", err), "Invalid identifier how to greet? in how to greet? = Hello \\x00263a"),
    ///     Err(err)                           => panic!("Wrong error returned (got {:?})", err),
    /// }
    /// ```
    pub fn parse_line(&mut self, line: &str) -> Result<(), Error> {
        let effective_line = line.trim_start();

        match effective_line.chars().next() {
            None | Some(';')    => Ok(()),
            Some(c) if c == '[' => self.parse_section(line),
            Some(_)             => self.parse_assignment(line),
        }
    }

    /// Parses an assignment ligne. An assignment is of form
    /// 
    /// ```ini
    /// identifier=value;comment
    /// ```
    /// 
    /// # Parameters
    /// `line` the line to parse
    /// 
    /// # Return value
    /// `Ok(())` in case of success
    /// 
    /// `Err(error)` in case of error with `error` as the error code
    fn parse_assignment(&mut self, line: &str) -> Result<(), Error> {
        // Getting the expression of `identifier` in "`identifier` = `value`[;comment]"
        let equal = match line.find('=') {
            Some(index) => index,
            None        => {
                let end_of_ident = line.trim_end().len();

                return Err(Error::from(ExpectedToken::new(String::from(line), end_of_ident, String::from("="))));
            }
        };

        let identifier = String::from(line[..equal].trim());

        // Getting the expression of `value` in "`identifier` = `value`[;comment]"
        let value = if line.len() == equal + 1 {
            ""
        } else {
            ignore_comment(&line[equal + 1..]).trim()
        };

        if !Identifier::is_valid(&identifier) {
            return Err(Error::from(InvalidIdentifier::new(String::from(line), identifier)));
        }
        let value = Value::parse(value)?;

        self.variables.insert(
            Identifier::new(self.cur_section.clone(), identifier),
            value,
        );
        Ok(())
    }

    /// Parses a section declaration. A section declaration is of form
    /// 
    /// ```ini
    /// [section];comment
    /// ```
    /// 
    /// # Parameters
    /// `line` the line to parse
    /// 
    /// # Return value
    /// `Ok(())` in case of success
    /// 
    /// `Err(error)` in case of error with `error` as the error code
    /// 
    /// # Panics
    /// Panics if line doesn't start with a `[` character, which indicates `line` is not a section declaration but may is a valid INI instruction. In this way, we can't return an error expecting a `[` at the beginning of the line, which doesn't make any sense
    fn parse_section(&mut self, line: &str) -> Result<(), Error> {
        let mut iter = line.char_indices();
        let leading_spaces = loop {
            match iter.next() {
                None => panic!("An INI section declaration starts with `[`. {} does not, which means the parser did not call the right function", line),
                Some((n, c)) => if c == '[' {
                    break n;
                } else if !c.is_whitespace() {
                    panic!("An INI section declaration starts with `[`. {} does not, which means the parser did not call the right function", line);
                },
            }
        };

        let mut end = 0;
        for (n, i) in iter.by_ref() {
            if i == ']' {
                end = n;
                break;
            }
        }

        // end == 0 means that there isn't any ']' while end == 1 means that the section name is empty
        if end == 0 {
            return Err(Error::from(ExpectedToken::new(String::from(line), leading_spaces, String::from("]"))));
        } else if end == 1 {
            return Err(Error::from(ExpectedIdentifier::new(String::from(line), leading_spaces + 1)));
        }

        let section = &line[leading_spaces + 1..end];
        if !Identifier::is_valid(section) {
            return Err(Error::from(InvalidIdentifier::new(String::from(line), String::from(section))));
        }

        // Checking integrity: I want to ensure there is no extra character after the section declaration
        // The only ones allowed are the whitespaces and the semicolon (with all the following ones)
        for (n, i) in iter {
            if i == ';' {
                break;
            } else if !i.is_whitespace() {
                let line = String::from(line);
                return Err(Error::from(UnexpectedToken::new(line, leading_spaces  // The leading spaces ignored
                                                                  + 2             // The '[' and ']' characters
                                                                  + section.len() // The identifier
                                                                  + n)));         // The index after the ']' character
            }
        }

        self.cur_section = Some(String::from(section));
        Ok(())
    }
}

/// Returns a subslice of the given slice which is comment-free (stopped at the first non-escaped semicolon ';'). `line` should be a single line
fn ignore_comment(line: &str) -> &str { 
    &line[..super::find_unescaped(line, ';').unwrap_or(line.len())]
}

/// Reads in an INI file and returns the parsed data
/// 
/// # Parameters
/// `path` the path of the file to open
/// 
/// # Return value
/// `Ok(data)` in case of success with `data` as a `HashMap<Identifier, Value>` linking each identifier to its associated value
/// 
/// `Err(error)` in case of failure with `error` as an error code for either an I/O error or a parsing error (see [ParseFileError](../errors/enum.ParseFileError.html "errors::ParseFileError"))
pub fn parse_file<T: AsRef<Path>>(path: T) -> Result<HashMap<Identifier, Value>, ParseFileError> {
    let mut file = File::open(path)?;

    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let content = content;

    let mut parser = Parser::new();

    let mut begin = 0;
    while begin < content.len() {
        let end = match &content[begin..].find('\n') {
            Some(val) => val + begin,
            None      => content.len(),
        };
        if begin == end {
            begin += 1;
            continue;
        }

        let line = &content[begin..end];
        parser.parse_line(line)?;

        begin = end + 1;
    }

    Ok(parser.data())
}


#[cfg(test)]
mod tests;
