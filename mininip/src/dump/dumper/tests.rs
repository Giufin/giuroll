use crate::dump::dumper::*;
use crate::datas::{Identifier, Value};

#[test]
fn dumper_without_globals() {
    let mut dumper = Dumper::new();

    let abc = Some(String::from("abc"));
    let a = Identifier::new(abc.clone(), String::from("a"));
    let b = Identifier::new(abc.clone(), String::from("b"));
    let c = Identifier::new(abc,         String::from("c"));

    let def = Some(String::from("def"));
    let d = Identifier::new(def.clone(), String::from("d"));
    let e = Identifier::new(def.clone(), String::from("e"));
    let f = Identifier::new(def,         String::from("f"));

    dumper.dump(a, Value::Int(1));
    dumper.dump(b, Value::Float(3.1415926535));
    dumper.dump(c, Value::Bool(true));
    dumper.dump(d, Value::Bool(false));
    dumper.dump(e, Value::Str(String::from("5")));
    dumper.dump(f, Value::Raw(String::from("abc")));

    let expected = "\
    [abc]\n\
    a=1\n\
    b=3.1415926535\n\
    c=on\n\
    \n\
    [def]\n\
    d=off\n\
    e='5'\n\
    f=abc\n";

    assert_eq!(expected, dumper.generate());
}

#[test]
fn dumper_with_globals() {
    let mut dumper = Dumper::new();

    let a = Identifier::new(None, String::from("a"));
    let b = Identifier::new(None, String::from("b"));
    let c = Identifier::new(None, String::from("c"));

    let def = Some(String::from("def"));
    let d = Identifier::new(def.clone(), String::from("d"));
    let e = Identifier::new(def.clone(), String::from("e"));
    let f = Identifier::new(def,         String::from("f"));

    dumper.dump(a, Value::Int(1));
    dumper.dump(b, Value::Float(3.1415926535));
    dumper.dump(c, Value::Bool(true));
    dumper.dump(d, Value::Bool(false));
    dumper.dump(e, Value::Str(String::from("5")));
    dumper.dump(f, Value::Raw(String::from("abc")));

    let expected = "\
    a=1\n\
    b=3.1415926535\n\
    c=on\n\
    \n\
    [def]\n\
    d=off\n\
    e='5'\n\
    f=abc\n";

    assert_eq!(expected, dumper.generate());
}

#[test]
fn dumper_with_escape() {
    let mut dumper = Dumper::new();

    let ident = Identifier::new(None, String::from("ident"));
    let val = Value::Raw(String::from(":D = \u{263a}"));

    dumper.dump(ident, val);

    assert_eq!("ident=\\:D \\= \\x00263a\n", dumper.generate());
}
