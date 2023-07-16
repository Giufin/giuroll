use crate::datas::*;
use crate::errors::Error;

#[test]
fn value_display() {
    let txt = "Hello world!";
    let val = Value::Raw(String::from(txt));

    assert_eq!(format!("{}", val), txt);
}

#[test]
fn value_dump() {
    let val = Value::Raw(String::from("très_content=☺ ; the symbol of hapiness"));
    let dumped = val.dump();

    assert_eq!(dumped, "tr\\x0000e8s_content\\=\\x00263a \\; the symbol of hapiness");
}

#[test]
fn value_parse_raw() {
    let val = Value::parse(r"Hello \x002665").unwrap();

    assert_eq!(val, Value::Raw(String::from("Hello \u{2665}")));
}

#[test]
fn value_parse_str_ok() {
    let val = Value::parse(r"'Hello world \x00263a'").unwrap();

    assert_eq!(val, Value::Str(String::from("Hello world \u{263a}")));
}

#[test]
fn value_parse_str_unclosed() {
    match Value::parse("'Hello world") {
        Ok(_)                        => panic!("This value is invalid and should not be accepted"),
        Err(Error::ExpectedToken(_)) => {},
        Err(err)                     => panic!("Invalid error value {:?}", err),
    }
}

#[test]
fn value_parse_int() {
    let val = Value::parse("666").unwrap();

    assert_eq!(val, Value::Int(666));
}

#[test]
fn value_parse_float() {
    let val = Value::parse("666.0").unwrap();

    assert_eq!(val, Value::Float(666.0));
}

#[test]
fn value_parse_bool_on() {
    let val = Value::parse("on").unwrap();

    assert_eq!(val, Value::Bool(true));
}

#[test]
fn value_parse_bool_enabled() {
    let val = Value::parse("enabled").unwrap();

    assert_eq!(val, Value::Bool(true));
}

#[test]
fn value_parse_bool_y() {
    let val = Value::parse("y").unwrap();

    assert_eq!(val, Value::Bool(true));
}

#[test]
fn value_parse_bool_yes() {
    let val = Value::parse("yes").unwrap();

    assert_eq!(val, Value::Bool(true));
}

#[test]
fn value_parse_bool_off() {
    let val = Value::parse("off").unwrap();

    assert_eq!(val, Value::Bool(false));
}

#[test]
fn value_parse_bool_disabled() {
    let val = Value::parse("disabled").unwrap();

    assert_eq!(val, Value::Bool(false));
}

#[test]
fn value_parse_bool_n() {
    let val = Value::parse("n").unwrap();

    assert_eq!(val, Value::Bool(false));
}

#[test]
fn value_parse_bool_no() {
    let val = Value::parse("no").unwrap();

    assert_eq!(val, Value::Bool(false));
}

#[test]
fn value_parse_err() {
    let val = Value::parse(r"Hello \p");

    assert!(val.is_err());
}

#[test]
fn identifier_new_some() {
    let section = Some(String::from("Section_name"));
    let variable = String::from("Variable_name");
    let ident = Identifier::new(section.clone(), variable.clone());

    assert_eq!(ident, Identifier { section, name: variable });
}

#[test]
fn identifier_new_none() {
    let section = None;
    let variable = String::from("Variable_name");
    let ident = Identifier::new(section.clone(), variable.clone());

    assert_eq!(ident, Identifier { section, name: variable });
}

#[test]
#[should_panic]
fn identifier_new_panics() {
    let section = Some(String::from("-Bad section name"));
    let variable = String::from("regular_name");
    let _ident = Identifier::new(section, variable);
}

#[test]
fn identifier_is_valid_full_test() {
    assert!(Identifier::is_valid("UPPERCASE_ONE"));
    assert!(Identifier::is_valid("lowercase_one"));
    assert!(Identifier::is_valid("alpha_numeric_42"));
    assert!(Identifier::is_valid("Contains spaces"));
    assert!(Identifier::is_valid("$is-valid_since:version~1.2.0"));
    assert!(!Identifier::is_valid("42_starts_with_a_digit"));
    assert!(!Identifier::is_valid("Non_ascii_character_\u{263a}"));
    assert!(!Identifier::is_valid("invalid_character="));
    assert!(!Identifier::is_valid(""));
}

#[test]
fn identifier_change_section_ok() {
    let mut ident = Identifier::new(Some(String::from("Section")), String::from("Variable"));

    ident.change_section(Some(String::from("Valid_one")));
}

#[test]
#[should_panic]
fn identifier_change_section_err() {
    let mut ident = Identifier::new(Some(String::from("Section")), String::from("Variable"));

    ident.change_section(Some(String::from("Inv@lid one")));
}

#[test]
fn identifier_change_section_none() {
    let mut ident = Identifier::new(Some(String::from("Section")), String::from("Variable"));

    ident.change_section(None);
}

#[test]
fn identifier_change_name_ok() {
    let mut ident = Identifier::new(Some(String::from("Section")), String::from("Variable"));

    ident.change_name(String::from("Valid_one"));
}

#[test]
#[should_panic]
fn identifier_change_name_err() {
    let mut ident = Identifier::new(Some(String::from("Section")), String::from("Variable"));

    ident.change_name(String::from("Inv@lid one"));
}

#[test]
fn identifier_format_with_section() {
    let section = String::from("Section");
    let variable = String::from("Variable");
    let ident = Identifier::new(Some(section.clone()), variable.clone());

    assert_eq!(format!("{}", ident), format!("{}.{}", section, variable));
}

#[test]
fn identifier_format_without_section() {
    let section = None;
    let variable = String::from("Variable");
    let ident = Identifier::new(section, variable.clone());

    assert_eq!(format!("{}", ident), variable);
}
