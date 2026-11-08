//! DM text: escapes, text macros (`\the`, `\red`, ...) and the codes BYOND
//! stores for embedded expressions. Strings arrive with their escapes still
//! raw, as `\x` pairs; line continuations are already gone.

use std::fmt;

use crate::operands::DMString;

#[derive(Debug)]
pub enum StringError {
    UnexpectedEnd,
    UnsupportedEscapeSequence(String),
}

impl fmt::Display for StringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringError::UnexpectedEnd => write!(f, "unexpected end"),
            StringError::UnsupportedEscapeSequence(seq) => {
                write!(f, "unsupported escape sequence \\{}", seq)
            }
        }
    }
}

/// A plain string literal.
pub(super) fn parse(data: &str) -> Result<DMString, StringError> {
    let mut builder = TextBuilder::new();
    builder.push(data)?;
    Ok(DMString(builder.buf))
}

/// Marks a value embedded with `[...]`.
const EMBED: u8 = 0x01;
/// An embedded value at the start of a sentence, which BYOND may capitalize.
const EMBED_SENTENCE: u8 = 0x02;
/// An embedded value after an article macro such as `\the`.
const EMBED_ARTICLE: u8 = 0x03;

/// Text macros and their codes. `space` says whether the macro swallows one
/// space after it.
struct Macro {
    name: &'static str,
    code: u8,
    space: bool,
    kind: MacroKind,
}

#[derive(PartialEq, Clone, Copy)]
enum MacroKind {
    /// Applies to the next embedded value (`\the [x]`).
    Article,
    /// Takes the embedded value that follows directly (`\ref[x]`).
    TakesEmbed,
    Other,
}

const MACROS: &[Macro] = &[
    Macro {
        name: "th",
        code: 5,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "a",
        code: 6,
        space: true,
        kind: MacroKind::Article,
    },
    Macro {
        name: "an",
        code: 6,
        space: true,
        kind: MacroKind::Article,
    },
    Macro {
        name: "A",
        code: 7,
        space: true,
        kind: MacroKind::Article,
    },
    Macro {
        name: "An",
        code: 7,
        space: true,
        kind: MacroKind::Article,
    },
    Macro {
        name: "the",
        code: 8,
        space: true,
        kind: MacroKind::Article,
    },
    Macro {
        name: "The",
        code: 9,
        space: true,
        kind: MacroKind::Article,
    },
    Macro {
        name: "he",
        code: 10,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "she",
        code: 10,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "He",
        code: 11,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "She",
        code: 11,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "his",
        code: 12,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "His",
        code: 13,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "hers",
        code: 14,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "Hers",
        code: 15,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "him",
        code: 16,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "her",
        code: 16,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "himself",
        code: 17,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "herself",
        code: 17,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "...",
        code: 18,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "s",
        code: 20,
        space: false,
        kind: MacroKind::Other,
    },
    Macro {
        name: "proper",
        code: 21,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "improper",
        code: 22,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "bold",
        code: 23,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "b",
        code: 23,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "italic",
        code: 24,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "i",
        code: 24,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "underline",
        code: 25,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "u",
        code: 25,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "strike",
        code: 26,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "red",
        code: 31,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "green",
        code: 32,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "blue",
        code: 33,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "black",
        code: 34,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "white",
        code: 35,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "yellow",
        code: 36,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "cyan",
        code: 37,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "magenta",
        code: 38,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "beep",
        code: 39,
        space: true,
        kind: MacroKind::Other,
    },
    Macro {
        name: "ref",
        code: 42,
        space: false,
        kind: MacroKind::TakesEmbed,
    },
    Macro {
        name: "icon",
        code: 43,
        space: false,
        kind: MacroKind::TakesEmbed,
    },
    Macro {
        name: "roman",
        code: 44,
        space: false,
        kind: MacroKind::TakesEmbed,
    },
    Macro {
        name: "Roman",
        code: 45,
        space: false,
        kind: MacroKind::TakesEmbed,
    },
];

/// Builds the bytes of a text literal, interpolated or not.
pub(super) struct TextBuilder {
    pub buf: Vec<u8>,
    /// Nothing but whitespace, quotes and HTML tags since the start or the
    /// end of the last sentence.
    sentence_start: bool,
    /// The last thing was an article macro.
    after_article: bool,
    /// The last thing was `\ref` or another macro that takes the next embed.
    embed_taken: bool,
    /// Inside an HTML tag, with what `sentence_start` was before it. A tag as
    /// a whole doesn't count as text, but what's inside it does.
    in_tag: Option<bool>,
    /// `sentence_start` at the start of the last tag.
    tag_start: bool,
    /// A colour macro (`\red`) came before.
    coloured: bool,
    embeds: u32,
}

impl TextBuilder {
    pub fn new() -> Self {
        Self {
            buf: vec![],
            sentence_start: true,
            after_article: false,
            embed_taken: false,
            in_tag: None,
            tag_start: true,
            coloured: false,
            embeds: 0,
        }
    }

    fn text_byte(&mut self, b: u8) {
        self.buf.push(b);
        // An article applies to the next embedded value, whatever text comes
        // first; \ref and the like only to one directly after them (tags
        // don't count)
        if self.in_tag.is_none() && b != b'<' {
            self.embed_taken = false;
        }
        // At the end of a tag the sentence is where it was at the tag's start;
        // a `>` without a tag goes back to the last tag's start (or the
        // string's)
        if b == b'>' {
            self.sentence_start = self.in_tag.take().unwrap_or(self.tag_start);
            return;
        }
        match b {
            // (a `<` inside a tag starts it over)
            b'<' => {
                self.in_tag = Some(self.sentence_start);
                self.tag_start = self.sentence_start;
                // (if another `<` starts the tag over, this one was text)
                self.sentence_start = false;
            }
            b' ' | b'\t' | b'\n' | b'\r' | b'"' | b'\'' => {}
            b'.' | b'!' | b'?' => self.sentence_start = true,
            _ => self.sentence_start = false,
        }
    }

    /// Text that never starts a sentence or a tag (escaped characters).
    fn literal_bytes(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
        self.after_article = false;
        self.embed_taken = false;
        self.sentence_start = false;
    }

    /// Marks where an embedded value goes.
    pub fn embed(&mut self) {
        self.embeds += 1;
        if self.embed_taken {
            self.embed_taken = false;
            self.sentence_start = false;
            return;
        }
        let code = if self.after_article {
            EMBED_ARTICLE
        } else if self.sentence_start && self.in_tag.is_none() {
            EMBED_SENTENCE
        } else {
            EMBED
        };
        self.buf.push(0xFF);
        self.buf.push(code);
        self.after_article = false;
        self.sentence_start = false;
    }

    pub fn push(&mut self, data: &str) -> Result<(), StringError> {
        let bytes = data.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let b = bytes[i];
            i += 1;
            if b != b'\\' {
                self.text_byte(b);
                continue;
            }
            let Some(&next) = bytes.get(i) else {
                return Err(StringError::UnexpectedEnd);
            };

            // \uXXXX: that character, in UTF-8
            if next == b'u' {
                let hex = data.get(i + 1..i + 5).filter(|h| h.chars().all(|c| c.is_ascii_hexdigit()));
                if let Some(c) = hex.and_then(|h| char::from_u32(u32::from_str_radix(h, 16).ok()?)) {
                    i += 5;
                    let mut utf8 = [0; 4];
                    for &b in c.encode_utf8(&mut utf8).as_bytes() {
                        self.text_byte(b);
                    }
                    continue;
                }
            }

            // Text macros, longest name first
            let rest = &data[i..];
            let found = MACROS
                .iter()
                .filter(|m| {
                    rest.starts_with(m.name)
                        && !rest[m.name.len()..]
                            .chars()
                            .next()
                            .is_some_and(|c| c.is_ascii_alphabetic())
                        && (m.kind != MacroKind::Article || rest[m.name.len()..].starts_with(' '))
                })
                .max_by_key(|m| m.name.len());
            if let Some(m) = found {
                i += m.name.len();
                let space_after = bytes.get(i) == Some(&b' ');
                let colour = (31..=38).contains(&m.code);
                // A colour after the first one (which ends the one before)
                // takes the space before it, if there is one, instead of the
                // one after
                let ends_colour = colour && std::mem::replace(&mut self.coloured, true);
                if ends_colour && space_after && self.buf.last() == Some(&b' ') {
                    self.buf.pop();
                } else if m.space && space_after {
                    i += 1;
                } else if m.kind == MacroKind::TakesEmbed && space_after {
                    // `\ref [x]`: the space goes before the macro, which takes
                    // the value right after it
                    i += 1;
                    self.buf.push(b' ');
                }
                // \th right after a value makes it an ordinal, in place of the
                // value's own code
                if m.name == "th"
                    && self.buf.len() >= 2
                    && self.buf[self.buf.len() - 2] == 0xFF
                    && matches!(
                        self.buf[self.buf.len() - 1],
                        EMBED | EMBED_SENTENCE | EMBED_ARTICLE
                    )
                {
                    self.buf.truncate(self.buf.len() - 2);
                }
                self.buf.push(0xFF);
                self.buf.push(m.code);
                // A colour (\red ... \magenta) counts as the character of
                // its code for where sentences start: \blue (33) is a `!`,
                // \green and \black (32, 34) a space and a `"`, the rest end
                // a sentence's start
                if (31..=38).contains(&m.code) {
                    match m.code {
                        33 => self.sentence_start = true,
                        32 | 34 => {}
                        _ => self.sentence_start = false,
                    }
                    continue;
                }
                self.after_article = m.kind == MacroKind::Article;
                self.embed_taken = m.kind == MacroKind::TakesEmbed;
                if m.kind != MacroKind::Article {
                    self.sentence_start = false;
                }
                continue;
            }

            i += 1;
            match next {
                b'n' => self.text_byte(b'\n'),
                b't' => self.text_byte(b'\t'),
                b'<' => self.literal_bytes(b"&lt;"),
                b'>' => self.literal_bytes(b"&gt;"),
                b'"' => self.text_byte(b'"'),
                b'\'' | b'\\' | b'[' | b']' | b'{' | b'}' => self.literal_bytes(&[next]),
                // `\ ` is nothing (it ends a line continuation's indent)
                b' ' => {}
                // \xHH: that byte
                b'x' if bytes.len() >= i + 2
                    && bytes[i..i + 2].iter().all(|b| b.is_ascii_hexdigit()) =>
                {
                    let byte = u8::from_str_radix(&data[i..i + 2], 16).unwrap();
                    i += 2;
                    self.text_byte(byte);
                }
                other => {
                    return Err(StringError::UnsupportedEscapeSequence(
                        (other as char).to_string(),
                    ))
                }
            }
        }
        Ok(())
    }
}
