//! Provides tools to generate a INI file from any data

/// Formats a `&str` by escaping special characters
/// 
/// # Return value
/// A `String` containing the escaped string
/// 
/// # Why should I format it?
/// The `Display` trait is about displaying a value to the user while `Debug` is for debuging. There is not any trait for dumping a value in a file knowing it can't be backed up in the same way it is displayed, so `escape` does this.
/// 
/// For instance, if `content` is `"a'bc=123;"`, then, `escape` will return `r"a\'bc\=123\;"` because it escapes special characters such as `=`, `'`, `;`, ...
/// 
/// More escaped characters may be found at [Wikipedia](https://en.wikipedia.org/wiki/INI_file#Escape_characters "INI file")
/// 
/// # The Unicode special case
/// A non-ASCII character is escaped as a `\x??????` with exactly 6 hexadecimal digits even if a smaller number is suitable
/// 
/// # Examples
/// ```
/// use mininip::dump::dump_str;
/// 
/// assert_eq!(dump_str("a'bc=123;"), r"a\'bc\=123\;");
/// assert_eq!(dump_str("\u{263a}"),  r"\x00263a");
/// ```
pub fn dump_str(content: &str) -> String {
    let mut new = String::with_capacity(content.len());

    for i in content.chars() {
        match i {
            // Those characters have a special rule to be escaped
            '\\'   => new.push_str(r"\\"),
            '\''   => new.push_str("\\'"),
            '"'    => new.push_str("\\\""),
            '\0'   => new.push_str("\\0"),
            '\x07' => new.push_str("\\a"),
            '\x08' => new.push_str("\\b"),
            '\t'   => new.push_str("\\t"),
            '\r'   => new.push_str("\\r"),
            '\n'   => new.push_str("\\n"),
            ';'    => new.push_str("\\;"),
            '#'    => new.push_str("\\#"),
            '='    => new.push_str("\\="),
            ':'    => new.push_str("\\:"),

            // The ASCII characters are left unchanged
            _ => new.push(i),
        }
    }

    new
}


mod dumper;
pub use dumper::*;

#[cfg(test)]
mod tests;
