//! An higher-level representation API for the data returned by parsers
//! 
//! # See
//! `Tree` to convert a `HashMap<Identifier, Value>` into a more user-friendly data-type
//! 
//! `Section` to list the keys inside a section

use crate::datas::{Identifier, Value};
use std::collections::{HashMap, hash_map};

/// A more user-friendly data-type to represent the data returned by `parser::Parser::data`
/// 
/// # Example
/// ```
/// use mininip::datas{Identifier, Value, self};
/// use datas::tree::Tree;
/// use mininip::parse::parse_file;
/// 
/// let tree = Tree::from_data(parse_file("good.ini").unwrap());
/// for i in tree.sections() {
///     println!("[{}] ; Section {}", i, i);
///     for j in i.keys() {
///         println!("{}={} ; key {}", j.ident().name(), j.value(), j.ident().name());
///     }
/// }
/// ```
pub struct Tree {
    cache: Cache,
    data: HashMap<Identifier, Value>,
}

impl Tree {
    /// Iterates over the sections of a `Tree`
    pub fn sections(&self) -> SectionIterator<'_> {
        SectionIterator {
            iterator: self.cache.sections.iter(),
            target: self,
            awaited: false,
        }
    }

    /// Returns an immutable reference to the owned data
    pub fn get_data(&self) -> &HashMap<Identifier, Value> {
        &self.data
    }

    /// Consumes `self` and returns the owned data
    pub fn into_data(self) -> HashMap<Identifier, Value> {
        self.data
    }
}

impl From<HashMap<Identifier, Value>> for Tree {
    fn from(data: HashMap<Identifier, Value>) -> Tree {
        Tree {
            cache: Cache::from(&data),
            data: data,
        }
    }
}


/// An iterator over sections in a `Tree`
pub struct SectionIterator<'a> {
    /// An iterator over the sections names in the `Tree`
    iterator: std::slice::Iter<'a, String>,
    /// The `Tree` owning the data iterated
    target: &'a Tree,
    /// Set to `true` if already awaited, `false` otherwise. Necessary because
    /// if it is set to `false`, we need to iterate over the global section
    /// before the named ones
    awaited: bool,
}

impl<'a> Iterator for SectionIterator<'a> {
    type Item = Section<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.awaited {
            self.awaited = true;

            if self.target.cache.keys.get(&None).is_some() {
                return Some(Section {
                    ident: None,
                    target: self.target,
                });
            }
        }

        let ident = self.iterator.next()?;
        Some(Section {
            ident: Some(&ident),
            target: self.target,
        })
    }
}


/// A section in a `Tree`
pub struct Section<'a> {
    ident: Option<&'a str>,
    target: &'a Tree,
}

impl<'a> Section<'a> {
    /// Returns the identifier (name) of this section
    pub fn name(&self) -> Option<&'a str> {
        self.ident.clone()
    }

    /// Returns an iterator over the keys of this section
    pub fn keys(&self) -> KeyIterator<'_> {
        KeyIterator {
            iterator: self.keys_internal_iterator(),
            target: self,
        }
    }

    /// Returns an iterator ofer the keys of this section.
    /// 
    /// # Note
    /// This iterator is the one internally used by the vector storing these key
    /// names. It must not be exposed in a public interface
    fn keys_internal_iterator(&self) -> std::slice::Iter<'a, String> {
        self.target.cache.keys[&self.name_owned()].iter()
    }

    /// Returns the identifier of this section like it must be passed to an
    /// `Identifier`: an `Option<String>` instead of an `Option<&str>`
    pub fn name_owned(&self) -> Option<String> {
        match self.ident {
            Some(val) => Some(String::from(val)),
            None      => None,
        }
    }
}


/// An iterator over keys in a given section
pub struct KeyIterator<'a> {
    iterator: std::slice::Iter<'a, String>,
    target: &'a Section<'a>,
}

impl<'a> Iterator for KeyIterator<'a> {
    type Item = Identifier;

    fn next(&mut self) -> Option<Self::Item> {
        let key = self.iterator.next()?;
        let section = self.target.name_owned();

        Some(Identifier::new(section, key.clone()))
    }
}


/// A cached result of an extraction of all the section and keys names. Will be
/// kept and updated forever in the owning `Tree`
struct Cache {
    /// An ordered list of sections
    sections: Vec<String>,
    /// A map associating a section name to an ordered list of key names
    keys: HashMap<Option<String>, Vec<String>>,
}

impl From<&HashMap<Identifier, Value>> for Cache {
    fn from(data: &HashMap<Identifier, Value>) -> Cache {
        let mut sections = Vec::new();
        let mut keys = HashMap::<_, Vec<String>>::new();

        for i in data.keys() {
            let section_name = match i.section() {
                Some(val) => Some(String::from(val)),
                None      => None,
            };

            match keys.entry(section_name.clone()) {
                hash_map::Entry::Occupied(mut entry) => entry.get_mut().push(String::from(i.name())),
                hash_map::Entry::Vacant(entry)       => {
                    let vec = vec![String::from(i.name())];
                    entry.insert(vec);

                    if let Some(val) = section_name {
                        sections.push(val);
                    }
                },
            }
        }

        // No collisions so unstable sorting is more efficient
        sections.sort_unstable();

        if let Some(val) = keys.get_mut(&None) {
            val.sort_unstable();
        }
        for i in &sections {
            keys.get_mut(&Some(i.clone()))
                .expect("Any section name in `section` should be in `keys`")
                .sort_unstable();
        }

        Cache {
            sections,
            keys,
        }
    }
}


#[cfg(test)]
mod tests;
