//! Provides a `Dumper` structure which creates a new INI file content

use crate::datas::{Identifier, Value};
use std::collections::{hash_map, HashMap};
use std::path::Path;
use std::fs::File;
use std::io::{self, Write};

/// A stated object, which from couples of [`Identifier`](../datas/struct.Identifier.html "datas::Identifier") and [`Value`](../datas/enum.Value.html "datas::Value"), creates a new INI tree, directly dumpable into a new file
/// 
/// # Example
/// ```
/// use mininip::dump::Dumper;
/// use mininip::datas::{Identifier, Value};
/// 
/// let mut dumper = Dumper::new();
/// 
/// let section = None;
/// let name = String::from("abc");
/// let abc = Identifier::new(section, name);
/// let value = Value::Raw(String::from("happy = \u{263a}"));
/// 
/// dumper.dump(abc, value);
/// 
/// let section = Some(String::from("maths"));
/// let name = String::from("sum");
/// let sum = Identifier::new(section, name);
/// let value = Value::Raw(String::from("\u{3a3}"));
/// 
/// dumper.dump(sum, value);
/// 
/// let expected = "\
/// abc=happy \\= \\x00263a\n\
/// \n\
/// [maths]\n\
/// sum=\\x0003a3\n";
/// 
/// assert_eq!(dumper.generate(), expected);
/// ```
#[derive(Debug)]
pub struct Dumper {
    /// The keys of this member are the section names and the values are a list of affectation lines generated
    tree: HashMap<Option<String>, Vec<String>>,
}

impl Dumper {
    /// Creates a new `Dumper` object
    pub fn new() -> Dumper {
        Dumper {
            tree: HashMap::new(),
        }
    }

    /// Dumps a couple [`Identifier`](../datas/struct.Identifier.html "datas::Identifier") / [`Value`](../datas/enum.Value.html "datas::Value") into `self`
    pub fn dump(&mut self, identifier: Identifier, value: Value) {
        let line = format!("{}={}", identifier.name(), value.dump());

        let key = match identifier.section() {
            Some(val) => Some(String::from(val)),
            None      => None,
        };
        match self.tree.entry(key) {
            hash_map::Entry::Occupied(mut entry) => entry.get_mut().push(line),
            hash_map::Entry::Vacant(entry)       => { entry.insert(vec![line]); },
        }
    }

    /// Generates a `String` containing the code of the INI data stored in the `Dumper`
    pub fn generate(mut self) -> String {
        // We want the sections to be sorted by name
        let mut sections: Vec<String> = Vec::with_capacity(self.tree.len());
        for (key, _value) in self.tree.iter() {
            if let Some(val) = key {
                sections.push(val.clone());
            }
        }
        sections.sort();

        // And None to be the first one
        let mut result = String::new();
        if let Some(val) = self.tree.get_mut(&None) {
            val.sort();
            for i in val {
                result.push_str(i);
                result.push('\n');
            }

            result.push('\n');
        }

        for i in sections {
            result.push('[');
            result.push_str(&i);
            result.push_str("]\n");

            let section = self.tree.get_mut(&Some(i))
                                   .expect("i is in sections so it is valid");
            section.sort();
            for j in section {
                result.push_str(j);
                result.push('\n');
            }

            result.push('\n');
        }

        result.pop();
        result
    }
}

/// Dumps a `HashMap<Identifier, Value>` into a file
/// 
/// # Parameters
/// `path` the path of the file (must be closed)
/// 
/// `data` the data to dump
/// 
/// # Return value
/// Since any [`Dumper`](struct.Dumper.html "dump::Dumper") operation is infallible, it only returns an `io::Result<()>` which indicates a file manipulation error
pub fn dump_into_file<T: AsRef<Path>>(path: T, data: HashMap<Identifier, Value>) -> io::Result<()> {
    let mut file = File::create(path)?;
    let mut dumper = Dumper::new();

    for (k, v) in data {
        dumper.dump(k, v);
    }

    file.write(dumper.generate().as_bytes())?;
    Ok(())
}


#[cfg(test)]
mod tests;
