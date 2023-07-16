# MinIniP

## What is MinIniP ?
### Presentation
**MinIniP** stands for Minimalist INI Parser. It is a parser written in Rust to
store datas in an easy and safe way. Currently, it is not intended to be
extremly fast and there is not any benchmark of it. You can use it like you want
in any project. Every change of one of the provided files must be published
under the MPL-2.0.

### Why MinIniP ?
Honnestly, there is not any particular reason to chose MinIniP. I just wrote it
to play with Rust wich I learn a few months ago. I will use it in my personnal
projects so it will be actively maintained for a long time.

### You convinced me ! How to use it ?

Just add

```toml
mininip="1.2"
```

to your `Cargo.toml` in the `dependencies` section and you are right ! You can
also download it at 
[the official repository](https://github.com/BorisDRYKONINGEN/mininip).

## What is a valid INI file ?
### A lack of standardisation
Since there is not any standard INI specification, each implementor writes its
own. Here is mine. You can contribute to the project by extending this
specification if you think something is missing. The only rule to follow is to
not break backward compatibility, except in one case: adding a new INI type may
break some use cases of the `Raw` type by moving a declaration of variable to
the new type.

For instance, you may create a `Set` INI type which is like the sets in maths,
and is enclosed by curly brackets `{}`. In this way, parsing this line

```ini
an INI key = { Hello, world, ! }
```

will no longer produce a `Value::Raw` value when parsing it but a `Value::Set`
instead.

### The specification followed by MinIniP
#### Identifiers
An identifier refers to either
* A section name
* A key name

An identifier must start with one of `.`, `$`, `:` or `a-zA-Z`. Since the second
character, all of `_`, `~`, `-`, `.`, `:`, `$`, `a-zA-Z` and `0-9` are allowed.
In the API, an `Identifier` refers to a combination of a section name and a key
name, so keep in mind *it is not just a key name* !

The specification above might be outdated, so refer to the generated
documentation (`Identifier::is_valid`) to be aware of what is a valid INI
identifier.

#### Values
##### Declaring a value
A value can be anything following the first `=` sign in an expression. It must
be assigned to a key and not a section. In this code

```ini
key=value
```

`key` must be a valid identifier and `value` is defined as the value.

##### Types
A value can be either

* `Raw` a valid value which does not match with any of the types below
* `Str` a valid value inside two quotes `'` or `"`
* `Int` a 64-bits-sized integer
* `Float` a 64-bits-sized floating-point number
* `Bool` a boolean (either `true` (`on`, `enabled`, `y` or `yes`) or `false` (`off`, `disabled`, `n` or `no`))

The highest priority is for the type `Str`. Since quotes are forbidden in all
the other use cases, a quoted value can only be a `Str`. Then, comes the `Bool`
type which only allows a few values (see above). Then, comes `Int` and in case
of failure while interpretting it as an integer, `Float`. If none of these types
match with the given value, the value is `Raw` which is the value as written in
the file (after unescaping, defined below).

##### Escape sequences
In an INI file, all the possible values are **not accepted**. For instance, you 
cannot store an emoji (like ☺ or ♥) or any other non-ASCII character *as is*
in a file. It is even true for characters which may be part of the INI syntax
like the semicolon `;`, the colon `:`, the equal sign `=`... However, you have
not to deal with these characters since MinIniP does it for you. The characters
are *escaped*. Here is an exhaustive list of the recognized escapes sequences.

| Non-escaped       | Escaped |
| :---------------- | :-----: |
| `\`               | `\\`    |
| `'`               | `\'`    |
| `"`               | `\"`    |
| null character    | `\0`    |
| bell / alert      | `\a`    |
| backspace         | `\b`    |
| tab character     | `\t`    |
| carriage return   | `\r`    |
| line feed         | `\n`    |
| `;`               | `\;`    |
| `#`               | `\#`    |
| `=`               | `\=`    |
| `:`               | `\:`    |
| unicode character | `\xxxxxx` with `xxxxxx` corresponding to its hexadecimal code (six digits) |

Please note that escapes are **not available** for identifiers.

#### Sections
A section refers to what can be called in Rust a module, or a namespace in C++.
In a few words, it is a named or anonymous set of keys. A section identifier
must be a valid identifier or nothing at all. A section is declared by putting
square brackets `[]` around its identifier on a line.

```ini
key_1 = value_1
key_2 = value_2

[section 1]
key_1 = value_3
key_2 = value_4

[section 2]
key_1 = value_5
key_2 = value_6
```

In this code, every occurences of `key_1` and `key_2` are different keys because
they are not in the same section. The first ones are in the global / anonymous
section, the two following ones in a `section 1`-named section, and the two last
ones in a `section 2`-named section.

In this API, section names are the first value stored in an `Identifier`. It is
a `Option<String>` since a section may be anonymous (the keys declared before
the first section are in the anonymous section corresponding to `None`). All the
named sections are represented as `Some(name)`. The second value is the key,
which is a `String` that must be a valid identifier.
