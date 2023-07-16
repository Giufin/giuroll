use crate::parse::*;
use crate::errors::Error;

#[test]
fn token_iterator_no_escapes() {
    let message = "Hello world!";
    let found = TokenIterator::from(message.chars())
                .collect::<Vec<Token>>();

    let expected = message.chars()
                          .map(|c| Token::Char(c))
                          .collect::<Vec<Token>>();

    assert_eq!(found, expected);
}

#[test]
fn token_iterator_special_escapes() {
    let message = "\\;\\:\\=\\\\\\\'\\\"";
    let found = TokenIterator::from(message.chars())
                .collect::<Vec<Token>>();

    let expected = [r"\;", r"\:", r"\=", r"\\", r"\'", "\\\""]
                   .iter()
                   .map(|s| Token::Escape(String::from(*s)))
                   .collect::<Vec<Token>>();

    assert_eq!(found, expected);
}

#[test]
fn token_iterator_unicode_escapes() {
    let message = r"\x00263a\x002665\x000100";
    let found = TokenIterator::from(message.chars())
                .collect::<Vec<Token>>();

    let expected = [r"\x00263a", r"\x002665", r"\x000100"]
                   .iter()
                   .map(|s| Token::Escape(String::from(*s)))
                   .collect::<Vec<Token>>();

    assert_eq!(found, expected);
}

#[test]
fn token_iterator_unfinished_escape() {
    let message = r"Hello\";
    let found = TokenIterator::from(message.chars())
                .collect::<Vec<Token>>();

    let mut expected = message.chars()
                              .take(message.len() - 1)
                              .map(|c| Token::Char(c))
                              .collect::<Vec<Token>>();
    expected.push(Token::Escape(String::from("\\")));

    assert_eq!(found, expected);
}

#[test]
fn parse_str_ignore() {
    let message = "Hello world";

    assert_eq!(message, parse_str(message).expect("This string is well escaped"));
}

#[test]
fn parse_str_special_escapes() {
    let message = "\\a\\b\\;\\:\\=\\'\\\"\\t\\r\\n\\0\\\\\\#";
    let expected = "\x07\x08;:='\"\t\r\n\0\\#";

    assert_eq!(parse_str(message).expect("This string is well escaped"), expected);
}

#[test]
fn parse_str_unicode_escapes() {
    let message = r"\x00263a\x002665\x000100";
    let expected = "\u{263a}\u{2665}\u{100}";

    assert_eq!(parse_str(message).expect("This string is well escaped"), expected);
}

#[test]
fn parse_str_unfinished_escape() {
    let message = r"Hello\";

    match parse_str(message) {
        Ok(_)                        => panic!("This string is ill-escaped and shouldn't be accepted"),
        Err(Error::InvalidEscape(_)) => {},
        Err(err)                     => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn parse_str_forbidden_ascii() {
    let message = r"hello=world";

    match parse_str(message) {
        Ok(_)                         => panic!("This string is ill-escaped and shouldn't be accepted"),
        Err(Error::ExpectedEscape(_)) => {},
        Err(err)                      => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn parse_str_forbidden_unicode() {
    let message = "☺";

    match parse_str(message) {
        Ok(_)                         => panic!("This string is ill-escaped and shouldn't be accepted"),
        Err(Error::ExpectedEscape(_)) => {},
        Err(err)                      => panic!("Wrong return value: {:?}", err),
    }
}

#[test]
fn find_unescaped_found() {
    let sequence = "abc";

    assert_eq!(Some(2), find_unescaped(sequence, 'c'));
}

#[test]
fn find_unescaped_ignore_simple_escape() {
    let sequence = "ab\\;cd;";

    assert_eq!(Some(6), find_unescaped(sequence, ';'));
}

#[test]
fn find_unescaped_ignore_unicode_escape() {
    let sequence = "ab\\x00263a0";

    assert_eq!(Some(10), find_unescaped(sequence, '0'));
}

#[test]
fn find_unescaped_not_found() {
    let sequence = "abcd";

    assert_eq!(None, find_unescaped(sequence, 'e'));
}
