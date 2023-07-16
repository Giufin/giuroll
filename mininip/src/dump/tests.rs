use crate::dump::*;

/// Tests only the constant substitutions such as `\` -> `\\` and not the runtime-computed ones
#[test]
fn dump_str_constants_substitutions() {
    assert_eq!(dump_str("\\"),   r"\\");
    assert_eq!(dump_str("'"),    String::from("\\'"));
    assert_eq!(dump_str("\""),   String::from("\\\""));
    assert_eq!(dump_str("\0"),   String::from("\\0"));
    assert_eq!(dump_str("\x07"), String::from("\\a"));
    assert_eq!(dump_str("\x08"), String::from("\\b"));
    assert_eq!(dump_str("\t"),   String::from("\\t"));
    assert_eq!(dump_str("\r"),   String::from("\\r"));
    assert_eq!(dump_str("\n"),   String::from("\\n"));
    assert_eq!(dump_str(";"),    String::from("\\;"));
    assert_eq!(dump_str("#"),    String::from("\\#"));
    assert_eq!(dump_str("="),    String::from("\\="));
    assert_eq!(dump_str(":"),    String::from("\\:"));
}

#[test]
fn dump_str_dynamic_substitutions() {
    assert_eq!(dump_str("\u{00263a}"), String::from("\\x00263a"));
    assert_eq!(dump_str("\u{000100}"), String::from("\\x000100"));
    assert_eq!(dump_str("\u{01342e}"), String::from("\\x01342e"));
}

#[test]
fn dump_str_ignore() {
    assert_eq!(dump_str("abc123"), String::from("abc123"));
}

#[test]
fn dump_str_complementary_test() {
    assert_eq!(dump_str("très_content=☺ ; the symbol of hapiness"), "tr\\x0000e8s_content\\=\\x00263a \\; the symbol of hapiness");
}

