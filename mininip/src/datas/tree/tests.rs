use crate::datas::{tree::*, Identifier, Value};
use crate::parse::Parser;

#[test]
fn cache_from_data() {
    let mut data = HashMap::new();

    let section = None;
    data.insert(Identifier::new(section.clone(), String::from("version")), Value::Str(String::from("1.3.0")));
    data.insert(Identifier::new(section.clone(), String::from("debug")), Value::Bool(true));
    data.insert(Identifier::new(section,         String::from("allow-errors")), Value::Bool(false));

    let section = Some(String::from("foo"));
    data.insert(Identifier::new(section.clone(), String::from("answer")), Value::Int(42));
    data.insert(Identifier::new(section,         String::from("pi")), Value::Float(3.14));

    let section = Some(String::from("bar"));
    data.insert(Identifier::new(section.clone(), String::from("baz")), Value::Raw(String::new()));
    data.insert(Identifier::new(section,         String::from("abc")), Value::Str(String::from("def")));

    let cache = Cache::from(&data);
    assert_eq!(&cache.sections, &vec![String::from("bar"), String::from("foo")]);

    let global = &cache.keys[&None];
    assert_eq!(global, &vec![String::from("allow-errors"), String::from("debug"), String::from("version")]);

    let foo = &cache.keys[&Some(String::from("foo"))];
    assert_eq!(foo, &vec![String::from("answer"), String::from("pi")]);

    let bar = &cache.keys[&Some(String::from("bar"))];
    assert_eq!(bar, &vec![String::from("abc"), String::from("baz")]);
}

#[test]
fn section_iterator_iterates_well() {
    // Here, we assume that the parser and the `Tree`'s constructor works well
    let mut parser = Parser::new();
    let content = "\
    version = '1.3.0'\n\
    debug = y\n\
    allow-errors = y\n\
    \n\
    [foo]\n\
    answer = 42\n\
    pi = 3.14\n\
    \n\
    [bar]\n\
    baz =\n\
    abc = \"def\"\n\
    ";

    for i in content.lines() {
        parser.parse_line(i)
            .expect("This code is valid");
    }

    let tree = Tree::from(parser.data());
    let expected = [None, Some("bar"), Some("foo")];

    for (n, i) in tree.sections().enumerate() {
        assert_eq!(&i.name(), &expected[n]);
    }
}

#[test]
fn key_iterator_iterates_well() {
    let mut data = HashMap::new();

    let section = None;
    data.insert(Identifier::new(section.clone(), String::from("version")), Value::Str(String::from("1.3.0")));
    data.insert(Identifier::new(section.clone(), String::from("debug")), Value::Bool(true));
    data.insert(Identifier::new(section,         String::from("allow-errors")), Value::Bool(false));

    let tree = Tree::from(data);
    let global = tree.sections()
                     .next()
                     .expect("This tree only owns one section");

    let expected = ["allow-errors", "debug", "version"];
    for (n, i) in global.keys().enumerate() {
        assert_eq!(i.name(), expected[n]);
        assert_eq!(i.section(), None);
    }
}

#[test]
fn key_iterator_no_global() {
    let mut data = HashMap::new();

    let section = Some(String::from("foo"));
    data.insert(Identifier::new(section.clone(), String::from("version")), Value::Str(String::from("1.3.0")));
    data.insert(Identifier::new(section.clone(), String::from("debug")), Value::Bool(true));
    data.insert(Identifier::new(section,         String::from("allow-errors")), Value::Bool(false));

    let tree = Tree::from(data);
    let foo = tree.sections()
                  .next()
                  .expect("There is one single section in this tree");

    assert_eq!(foo.name(), Some("foo"));
}
