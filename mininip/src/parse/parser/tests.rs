use crate::parse::*;
use crate::datas::{Identifier, Value};
use crate::errors::Error;

#[test]
fn parser_parse_assignment_simplest() {
    let expr = "ident=val";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_commented() {
    let expr = "ident=val;This is a comment";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_with_spaces() {
    let expr = "ident = val";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_with_comment_and_spaces() {
    let expr = "ident=val ; This is a comment";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_with_leading_spaces() {
    let expr = "    ident=val";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_unicode_value() {
    let expr = r"latin_small_letter_e_with_acute=\x0000e9";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("latin_small_letter_e_with_acute"));
    let val = Value::Raw(String::from("\u{e9}"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_unicode_comment() {
    let expr = "ident=val; C'est un cas tout à fait valid"; // Notice the 'à' in the comment
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_str() {
    let expr = "ident='Hello world!'";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Str(String::from("Hello world!"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_int() {
    let expr = "ident=0";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Int(0);
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_float() {
    let expr = "ident=0.0";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Float(0.0);
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_bool() {
    let expr = "ident=on";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Bool(true);
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_assignment_unicode_identifier() {
    let expr = r"é=\x0000e9";
    let mut parser = Parser::new();

    match parser.parse_assignment(expr) {
        Ok(())                           => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::InvalidIdentifier(_)) => {},
        Err(err)                         => panic!("Wrong return value for this error: {:?}", err),
    }
}

#[test]
fn parser_parse_assignment_bad_ident() {
    let expr = "my*identifier=val";
    let mut parser = Parser::new();

    match parser.parse_assignment(expr) {
        Ok(())                           => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::InvalidIdentifier(_)) => {},
        Err(err)                         => panic!("Wrong return value for this error: {:?}", err),
    }
}

#[test]
fn parser_parse_assignment_bad_value() {
    let expr = "ident=abc=123";
    let mut parser = Parser::new();

    match parser.parse_assignment(expr) {
        Ok(())                        => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::ExpectedEscape(_)) => {},
        Err(err)                      => panic!("Wrong return value for this error: {:?}", err),
    }
}

#[test]
fn parser_parse_assignment_no_value() {
    let expr = "ident=";
    let mut parser = Parser::new();

    parser.parse_assignment(expr)
        .expect("This code should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::new());
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_section_simplest() {
    let expr = "[section]";
    let mut parser = Parser::new();

    parser.parse_section(expr)
        .expect("This code should be accepted because it's a valid INI section declaration");
    
    assert_eq!(parser.cur_section, Some(String::from("section")));

    parser.parse_assignment("ident=val").unwrap();

    let data = parser.data();
    let key = Identifier::new(Some(String::from("section")), String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_section_with_comment() {
    let expr = "[section];comment";
    let mut parser = Parser::new();

    parser.parse_section(expr)
        .expect("This code should be accepted because it's a valid INI section declaration");
    
    assert_eq!(parser.cur_section, Some(String::from("section")));

    parser.parse_assignment("ident=val").unwrap();

    let data = parser.data();
    let key = Identifier::new(Some(String::from("section")), String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_section_with_comment_and_whitespaces() {
    let expr = "[section]\t ; comment";
    let mut parser = Parser::new();

    parser.parse_section(expr)
        .expect("This code should be accepted because it's a valid INI section declaration");
    
    assert_eq!(parser.cur_section, Some(String::from("section")));

    parser.parse_assignment("ident=val").unwrap();

    let data = parser.data();
    let key = Identifier::new(Some(String::from("section")), String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_section_with_leading_spaces() {
    let expr = "    [section]";
    let mut parser = Parser::new();

    parser.parse_section(expr)
        .expect("This code should be accepted because it's a valid INI section declaration");

    assert_eq!(parser.cur_section, Some(String::from("section")));

    parser.parse_assignment("ident=val").unwrap();

    let data = parser.data();
    let key = Identifier::new(Some(String::from("section")), String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
#[should_panic]
fn parser_parse_section_leading_extra_token() {
    let expr = "char nullTerminatedString[BUFSIZ]";
    let mut parser = Parser::new();

    std::mem::drop(parser.parse_section(expr));
}

#[test]
fn parser_parse_section_ending_extra_token() {
    let expr = "[section] () -> bool { return true; }";
    let mut parser = Parser::new();

    match parser.parse_section(expr) {
        Ok(())                         => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::UnexpectedToken(_)) => {},
        Err(err)                       => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn parser_parse_section_invalid_identifier() {
    let expr = "[hello there!]";
    let mut parser = Parser::new();

    match parser.parse_section(expr) {
        Ok(())                           => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::InvalidIdentifier(_)) => {},
        Err(err)                         => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn parser_parse_section_empty() {
    let expr = "[]";
    let mut parser = Parser::new();

    match parser.parse_section(expr) {
        Ok(())                            => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::ExpectedIdentifier(_)) => {},
        Err(err)                          => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn parser_parse_section_unterminated() {
    let expr = "[EOF";
    let mut parser = Parser::new();

    match parser.parse_section(expr) {
        Ok(())                       => panic!("This code is wrong and shouldn't be accepted"),
        Err(Error::ExpectedToken(_)) => {},
        Err(err)                     => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn parser_parse_line_assignment() {
    let expr = "ident = val";
    let mut parser = Parser::new();

    parser.parse_line(expr)
        .expect("This line should be accepted because it's a valid INI assignment");

    let data = parser.data();
    let key = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from("val"));
    assert_eq!(data[&key], val);
}

#[test]
fn parser_parse_line_section() {
    let expr = "[section]";
    let mut parser = Parser::new();

    parser.parse_line(expr)
        .expect("This line should be accepted because it's a valid INI section declaration");

    assert_eq!(parser.cur_section, Some(String::from("section")));
}

#[test]
fn parser_parse_line_comment() {
    let expr = "; Just a comment";
    let mut parser = Parser::new();

    parser.parse_line(expr)
        .expect("This line should be accepted because it's a valid INI comment");
}

#[test]
fn parser_parse_line_empty() {
    let expr = "";
    let mut parser = Parser::new();

    parser.parse_line(expr)
        .expect("This line should be accepted because it's a valid INI empty line");
}
