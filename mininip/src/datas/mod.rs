//! The basic datas structures like [`Identifier`](datas/struct.Identifier.html "Identifier") and [`Value`](datas/enum.Value.html "Value")

use std::fmt::{self, Display, Formatter};
use crate::{parse, dump};
use crate::errors::{Error, error_kinds::*};

/// The value of a INI variable
/// 
/// The following types are available
/// - `Raw`: the raw content of the file, not formatted. The only computation is that the escaped characters are unescaped (see [parse_str](../parse/fn.parse_str.html "parse::parse_str") to learn more about escaped characters)
/// - `Str`: a quoted string written inside non-escaped quotes like that `"Hello world!"` or that `'Hello world!'`
/// - `Int`: a 64 bytes-sized integer
/// - `Float`: a 64 bytes-sized floating-point number
/// - `Bool`: a boolean (currently either `on` or `off`)
/// 
/// Each type is represented as an enum variant
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Raw(String),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Value::Raw(string)   => string.fmt(formatter),
            Value::Str(string)   => string.fmt(formatter),
            Value::Int(number)   => number.fmt(formatter),
            Value::Float(number) => number.fmt(formatter),
            Value::Bool(true)    => "on".fmt(formatter),
            Value::Bool(false)   => "off".fmt(formatter),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Raw(String::new())
    }
}

impl Value {
    /// Builds a new [`Value`](enum.Value.html "datas::Value") from `content`, an INI-formatted string
    /// 
    /// # Return value
    /// `Ok(value)` with `value` as the new object
    /// 
    /// `Err(error)` when an error occurs while parsing `content` with `error` as the error code
    pub fn parse(content: &str) -> Result<Value, Error> {
        let effective = content.trim();

        if effective.starts_with("'") || effective.starts_with("\"") {
            let quote = &effective[..1];

            if !effective.ends_with(quote) {
                let err = ExpectedToken::new(String::from(content), content.len(), String::from(quote));
                Err(Error::from(err))
            } else {
                let parsed = parse::parse_str(&effective[1..effective.len() - 1])?;
                Ok(Value::Str(parsed))
            }
        }

        else if effective == "on" || effective == "enabled" || effective == "y" || effective == "yes" {
            Ok(Value::Bool(true))
        } else if effective == "off" || effective == "disabled" || effective == "n" || effective == "no" {
            Ok(Value::Bool(false))
        }

        else if let Ok(value) = effective.parse::<i64>() {
            Ok(Value::Int(value))
        }

        else if let Ok(value) = effective.parse::<f64>() {
            Ok(Value::Float(value))
        }

        else {
            Ok(Value::Raw(parse::parse_str(effective)?))
        }
    }

    /// Formats `self` to be dumped in an INI file
    /// 
    /// It means that `format!("{}={}", ident, value.dump())` with `ident` as a valid key and `value` a [`Value`](enum.Value.html "Value") can be properly registered and then, parsed as INI
    /// 
    /// # Return value
    /// A `String` containing the value of `self` once formatted
    /// 
    /// # See
    /// See [`dump_str`](fn.dump_str.html "datas::dump_str") for more informations about this format
    /// 
    /// # Note
    /// `self` is backed up in a way preserving its type
    /// 
    /// - `Raw` is backed up as is, once escaped
    /// - `Str` is backed up with two quotes `'` or `"` around its value once escaped
    /// - `Int` is backed up as is
    /// - `Float` is backed up as is
    /// - `Bool` is backed up as two different values: `true` and `false`
    /// 
    /// # Examples
    /// ```
    /// use mininip::datas::Value;
    /// 
    /// let val = Value::Str(String::from("très_content=☺ ; the symbol of hapiness"));
    /// let dumped = val.dump();
    /// 
    /// assert_eq!(dumped, "'tr\\x0000e8s_content\\=\\x00263a \\; the symbol of hapiness'"); // Notice the quotes here
    /// ```
    pub fn dump(&self) -> String {
        match self {
            Value::Raw(string)   => format!("{}", dump::dump_str(&string)),
            Value::Str(string)   => format!("'{}'", dump::dump_str(&string)),
            Value::Int(number)   => format!("{}", number),
            Value::Float(number) => format!("{}", number),
            Value::Bool(true)    => String::from("on"),
            Value::Bool(false)   => String::from("off"),
        }
    }
}


/// The identifier of a variable, which is its identity. Of course, this type is `Hash` because it may be used as a key in a `HashMap`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    section: Option<String>,
    name: String,
}

impl Identifier {
    /// Creates an identifier with a valid section name and a valid name
    /// 
    /// # Panics
    /// Panics if either `section` or `name` is an invalid identifier according to [`Identifier::is_valid`](struct.Identifier.html#method.is_valid "datas::Identifier::is_valid")
    pub fn new(section: Option<String>, name: String) -> Identifier {
        if let Some(section) = &section {
            assert!(Identifier::is_valid(section));
        }
        assert!(Identifier::is_valid(&name));

        Identifier {
            section,
            name,
        }
    }

    /// Returns `true` if the given string is a valid identifier and `false` otherwise
    /// 
    /// A valid identifier is defined as a string of latin alphanumeric characters and any of `_`, `~`, `-`, `.`, `:`, `$` and space starting with a latin alphabetic one or any of `.`, `$` or `:`. All of these characters must be ASCII
    /// 
    /// # Notes
    /// Since the INI file format is not really normalized, this definition may evolve in the future. In fact, I will avoid when possible to make a stronger rule, in order to keep backward compatibility
    /// 
    /// # Examples
    /// ```
    /// use mininip::datas::Identifier;
    /// 
    /// assert!(Identifier::is_valid("identifier"));
    /// assert!(Identifier::is_valid("digits1230"));
    /// assert!(Identifier::is_valid("UPPERCASE_AND_UNDERSCORES"));
    /// assert!(Identifier::is_valid("contains spaces inside"));
    /// assert!(!Identifier::is_valid("123_starts_with_a_digit"));
    /// assert!(!Identifier::is_valid("invalid_characters;!\\~"));
    /// assert!(!Identifier::is_valid("é_is_unicode"));
    /// assert!(!Identifier::is_valid(" starts_with_a_space"));
    /// ```
    pub fn is_valid(ident: &str) -> bool {
        let mut iter = ident.chars();
        match iter.next() {
            // An empty string is not allowed
            None    => return false,

            // The first character must be a letter, a point, a dollar sign or a colon
            Some(c) => if !c.is_ascii() || !c.is_alphabetic() && c != '.' && c != '$' && c != ':' {
                return false;
            },
        }

        for i in iter {
            // The following ones may be numeric characters, underscores, tildes, dashs or spaces
            if !i.is_ascii() || !i.is_alphanumeric()
                             && i != '_' && i != '~' 
                             && i != '-' && i != '.'
                             && i != ':' && i != '$'
                             && i != ' ' {
                return false;
            }
        }

        true
    }

    /// Returns the name of the variable as a reference
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the section of the variable which may be a named section as `Some(name)` or the "global scope" wich is `None`
    pub fn section(&self) -> Option<&str> {
        match &self.section {
            Some(val) => Some(&val),
            None      => None,
        }
    }

    /// Change the name of the variable
    /// 
    /// # Panics
    /// Panics if `name` is invalid according to [`Identifier::is_valid`](struct.Identifier.html#method.is_valid "datas::Identifier::is_valid")
    pub fn change_name(&mut self, name: String) {
        assert!(Identifier::is_valid(&name));

        self.name = name;
    }

    /// Changes the section of the variable. `section` may be `Some(name)` with `name` as the name of the section of `None` for the "global scope"
    /// 
    /// # Panics
    /// Panics if `section` is invalid according to [`Identifier::is_valid`](struct.Identifier.html#method.is_valid "datas::Identifier::is_valid")
    pub fn change_section(&mut self, section: Option<String>) {
        if let Some(section) = &section {
            assert!(Identifier::is_valid(section));
        }

        self.section = section;
    }
}

impl Display for Identifier {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        if let Some(section) = &self.section {
            formatter.write_str(&section)?;
            formatter.write_str(".")?;
        }

        formatter.write_str(&self.name)
    }
}


pub mod tree;

#[cfg(test)]
mod tests;
