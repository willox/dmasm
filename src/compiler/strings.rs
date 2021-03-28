use crate::operands::DMString;

#[derive(Debug)]
pub enum StringError {
    UnexpectedEnd,
    UnsupportedEscapeSequence,
}

pub(super) fn parse(data: &str) -> Result<DMString, StringError> {
    let mut builder = StringBuilder::new();
    builder.push(data.as_bytes())?;
    Ok(DMString(builder.buf))
}

// Tracks what the next interpolated value will be treated as
struct StringBuilder {
    buf: Vec<u8>,
}

impl StringBuilder {
    fn new() -> StringBuilder {
        Self { buf: vec![] }
    }

    fn escape_sequence(
        &mut self,
        it: &mut std::iter::Peekable<std::slice::Iter<u8>>,
    ) -> Result<(), StringError> {
        match it.next() {
            Some(ch) => {
                match ch {
                    // TODO: We're missing a bunch
                    // TODO: The parser doesn't do well here
                    b'\\' => self.buf.push(b'\\'),
                    b't' => self.buf.push(b'\t'),
                    b'n' => self.buf.push(b'\n'),
                    b'"' => self.buf.push(b'"'), // ???
                    b'<' => self.buf.extend_from_slice(b"&amp;lt;"),
                    b'>' => self.buf.extend_from_slice(b"&amp;gt;"),
                    b' ' => (),
                    b'\n' => (),

                    _ => return Err(StringError::UnsupportedEscapeSequence),
                }
            }

            None => return Err(StringError::UnexpectedEnd),
        }

        Ok(())
    }

    fn push(&mut self, data: &[u8]) -> Result<(), StringError> {
        let mut it = data.into_iter().peekable();

        loop {
            match it.next() {
                // Escape sequences
                Some(b'\\') => self.escape_sequence(&mut it)?,

                // Anything else
                Some(ch) => {
                    self.buf.push(*ch);
                }

                None => break,
            }
        }

        Ok(())
    }
}
