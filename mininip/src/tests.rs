use crate::{parse, dump, errors};
use parse::parse_file;
use errors::ParseFileError;
use crate::datas::{Identifier, Value};
use std::collections::HashMap;
use dump::dump_into_file;
use std::fs::{self, File};
use std::io::Read;

#[test]
fn parse_reverses_dump() {
    let message = "Hello world ☺. 1+1=2; 2+2=4 \\0/";

    assert_eq!(parse::parse_str(&dump::dump_str(message)).expect("`dump_str` must return a well escaped string"), message);
}

#[test]
fn parse_good_file() {
    let data = parse_file("good.ini").unwrap();

    let author = Identifier::new(None, String::from("author"));
    let version_major = Identifier::new(None, String::from("version_major"));

    let numbers = Some(String::from("numbers"));
    let one = Identifier::new(numbers.clone(), String::from("one"));
    let two = Identifier::new(numbers.clone(), String::from("two"));
    let three = Identifier::new(numbers, String::from("three"));

    let symbols = Some(String::from("symbols"));
    let smiley = Identifier::new(symbols.clone(), String::from("smiley"));
    let semicolon = Identifier::new(symbols, String::from("semicolon"));

    let valid_since_1_2_0 = Some(String::from("valid since 1.2.0"));
    let contains_spaces = Identifier::new(valid_since_1_2_0.clone(), String::from("contains spaces"));
    let starts_with_dollar = Identifier::new(valid_since_1_2_0.clone(), String::from("$starts-with-$"));
    let contains_colon = Identifier::new(valid_since_1_2_0, String::from("contains:"));

    println!("{:?}", data);

    assert_eq!(data[&author], Value::Str(String::from("Boris DRYKONINGEN")));
    assert_eq!(data[&version_major], Value::Int(0));

    assert_eq!(data[&one], Value::Int(1));
    assert_eq!(data[&two], Value::Int(2));
    assert_eq!(data[&three], Value::Int(3));

    assert_eq!(data[&smiley], Value::Raw(String::from("\u{263a}")));
    assert_eq!(data[&semicolon], Value::Raw(String::from(";")));

    assert_eq!(data[&contains_spaces], Value::Bool(true));
    assert_eq!(data[&starts_with_dollar], Value::Bool(true));
    assert_eq!(data[&contains_colon], Value::Bool(true));
}

#[test]
fn parse_bad_file() {
    let err = parse_file("bad.ini");
    match err {
        Ok(_)                              => panic!("This file contains wrong code and shouldn't be allowed"),
        Err(ParseFileError::ParseError(_)) => {},
        Err(err)                           => panic!("Wrong error value returned: {:?}", err),
    }
}

#[test]
fn parse_non_existing_file() {
    let err = parse_file("This file shouldn't exist. If you see it, remove it now.ini");
    match err {
        Ok(_)                           => panic!("This file does not exist. If it exists, remove it"),
        Err(ParseFileError::IOError(_)) => {},
        Err(err)                        => panic!("Wrong error value returned: {:?}", err),
    }
}

#[test]
fn test_dump_into_file() {
    let mut data = HashMap::new();

    let insert = &mut |section, ident, val| {
        let ident = Identifier::new(section, String::from(ident));
        data.insert(ident, val);
    };

    let section = None;
    insert(section.clone(), "abc", Value::Int(123));
    insert(section.clone(), "def", Value::Int(456));
    insert(section,         "ghi", Value::Int(789));

    let section = Some(String::from("maths"));
    insert(section.clone(), "sum",      Value::Str(String::from("∑")));
    insert(section.clone(), "sqrt",     Value::Str(String::from("√")));
    insert(section,         "infinity", Value::Str(String::from("∞")));

    let path = "test dump.ini";
    dump_into_file(path, data).unwrap();

    let expected = "\
    abc=123\n\
    def=456\n\
    ghi=789\n\
    \n\
    [maths]\n\
    infinity='\\x00221e'\n\
    sqrt='\\x00221a'\n\
    sum='\\x002211'\n";

    let mut file = File::open(path)
        .expect("Created above");
    let mut content = String::with_capacity(expected.len());
    file.read_to_string(&mut content).unwrap();

    std::mem::drop(file);
    if let Err(err) = fs::remove_file(path) {
        eprintln!("Error while removing the file: {}", err);
    }

    assert_eq!(content, expected);
}
