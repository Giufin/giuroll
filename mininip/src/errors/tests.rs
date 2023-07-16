use crate::errors::*;

#[test]
fn expected_identifier_format() {
    let line = String::from("=hello");
    let err = error_kinds::ExpectedIdentifier::new(line, 0);

    let fmt = format!("{}", err);
    assert_eq!(fmt, "Expected identifier {here}=hello");
}

#[test]
fn expected_token_format() {
    let line = String::from("hello world");
    let err = error_kinds::ExpectedToken::new(line, 5, String::from("="));

    let fmt = format!("{}", err);
    assert_eq!(fmt, "Expected = hello{here} world");
}

#[test]
fn expected_escape_format() {
    let line = String::from("greet = hello \u{263a}");
    let err = error_kinds::ExpectedEscape::new(line, 14, String::from("\\x00263a"));

    let fmt = format!("{}", err);
    assert_eq!(fmt, "Expected escape sequence \\x00263a instead of \u{263a} in greet = hello {here}");
}

#[test]
fn unexpected_token_format() {
    let line = String::from("ident\\x002665 = value");
    let err = error_kinds::UnexpectedToken::new(line, 5);

    let fmt = format!("{}", err);
    assert_eq!(fmt, "Unexpected token \\ ident{here}");
}

#[test]
fn invalid_escape_format() {
    let line = String::from("ident=\\xyzabcd");
    let err = error_kinds::InvalidEscape::new(line, String::from("\\xyzabcd"));

    let fmt = format!("{}", err);
    assert_eq!(fmt, "Invalid escape sequence \\xyzabcd in ident=\\xyzabcd");
}

#[test]
#[should_panic]
fn expected_identifier_overflow() {
    let line = String::from("[]; a non-named section");
    let _err = error_kinds::ExpectedIdentifier::new(line, 1_000_000);
}

#[test]
#[should_panic]
fn expected_token_overflow() {
    let line = String::from("hello world");
    let _err = error_kinds::ExpectedToken::new(line, 1_000_000, String::from("="));
}

#[test]
#[should_panic]
fn expected_escape_overflow() {
    let line = String::from("hello world");
    let _err = error_kinds::ExpectedEscape::new(line, 1_000_000, String::from("\\x00263a"));
}

#[test]
#[should_panic]
fn expected_escape_alignment_error() {
    let line = String::from("greet = hello \u{263a}");
    // 15 is not an overflow but it's the second byte of ☺ (the last character)
    let _err = error_kinds::ExpectedEscape::new(line, 15, String::from("\\x00263a"));
}

#[test]
#[should_panic]
fn unexpected_token_overflow() {
    let line = String::from("hello world");
    let _err = error_kinds::UnexpectedToken::new(line, 1000_000);
}

#[test]
#[should_panic]
fn unexpected_token_alignment_error() {
    let line = String::from("greet = hello \u{263a}");
    // 15 is not an overflow but it's the second byte of ☺ (the last character)
    let _err = error_kinds::UnexpectedToken::new(line, 15);
}

#[test]
#[should_panic]
fn invalid_escape_not_included() {
    let line = String::from("ident=\\xyzabcd");
    let _err = error_kinds::InvalidEscape::new(line, String::from("\\{"));
}

#[test]
fn nth_char_works_well() {
    assert_eq!(nth_char("abcdefg", 1), 'b');
}

#[test]
#[should_panic]
fn nth_char_alignment_error_middle() {
    let _char = nth_char("hello \u{263a} world", 7);
}

#[test]
#[should_panic]
fn nth_char_alignment_error_end() {
    let _char = nth_char("hello \u{263a}", 7);
}

#[test]
#[should_panic]
fn nth_char_overflow() {
    let _char = nth_char("hello", 1_000_000);
}
