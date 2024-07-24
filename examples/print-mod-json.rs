use serde_json::{json, Result, Value};
fn main() {
    let mut v = json! ({
        "name": "giuroll",
        "author": "AUTHOR",
        "priority": "1",
        "description": "New rollback netcode.\nAlso provides replay rewind and takeover.",
        "descriptionI18n": [],
        "notes": "",
        "notesI18n": [],
        "icon": null,
        "banner": null,
        "version": "VERSION",
        "fileName": "giuroll.dll",
        "main": "giuroll.dll",
        "configFiles": [
            "giuroll.ini"
        ],
        "updateWorkingDir": ".",
        "fromLocalArchive": true,
        "compressed": true
    });
    v["author"] = Value::String(
        env!("CARGO_PKG_AUTHORS")
            .split(":")
            .collect::<Vec<_>>()
            .join(", "),
    );
    v["version"] = Value::String(env!("CARGO_PKG_VERSION").to_string());
    println!("{}", v.to_string());
}
