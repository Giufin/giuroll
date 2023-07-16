//! An minimalist ini file parser (MinIniP stands for Minimalist Ini Parser). It is written in Rust but I would export its API to the C programming language in order to make various bindings

pub mod datas;
pub mod dump;
pub mod parse;
pub mod errors;

#[cfg(test)]
mod tests;
