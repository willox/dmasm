use crate::operands;
use crate::Instruction;
use crate::Node;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, line_ending, multispace0, space0},
    combinator::{eof, map, recognize},
    error::{convert_error, FromExternalError, ParseError, VerboseError},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
    Err, IResult,
};

pub fn parse_identifier<'a, E>(i: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(i)
}

fn parse_label<'a, E>(i: &'a str) -> IResult<&'a str, Node, E>
where
    E: ParseError<&'a str>,
{
    map(
        delimited(
            space0,
            parse_identifier,
            pair(char(':'), alt((line_ending, eof))),
        ),
        |x: &str| Node::Label(operands::Label::Named(x.into())),
    )(i)
}

fn parse_comment<'a, E>(i: &'a str) -> IResult<&'a str, Node, E>
where
    E: ParseError<&'a str>,
{
    map(
        preceded(char(';'), take_while(|x| x != '\r' && x != '\n')),
        |x: &str| Node::Comment(x.into()),
    )(i)
}

pub fn whitespace<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    F: 'a + Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn parse_nodes<'a, E>(i: &'a str) -> IResult<&'a str, Vec<Node>, E>
where
    E: 'a + ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    terminated(
        many0(delimited(
            multispace0,
            alt((
                parse_label,
                parse_comment,
                map(Instruction::deserialize, |x| Node::Instruction(x, ())),
            )),
            multispace0,
        )),
        pair(multispace0, eof),
    )(i)
}

pub fn parse(asm: &str) -> Result<Vec<Node>, String> {
    let x = parse_nodes::<VerboseError<&str>>(asm)
        .map(|(_, y)| y)
        .map_err(|x| match x {
            Err::Error(e) | Err::Failure(e) => convert_error(asm, e),
            _ => panic!(),
        });

    x
}

/*
fn parse_node<'a, E>(mut i: &'a str) -> IResult<&str, &str, E>
where
    E: ParseError<&'a str>,
{
    terminated(parse_instruction, alt((line_ending, eof)))(i)
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{error::ErrorKind, Err};

    #[test]
    fn test_label() {
        assert_eq!(
            parse_label::<(_, ErrorKind)>("Invalid"),
            Err(Err::Error(("", ErrorKind::Char)))
        );
        assert_eq!(
            parse_label::<(_, ErrorKind)>("Loop:"),
            Ok(("", Node::Label(operands::Label::Named("Loop".into()))))
        );
        assert_eq!(
            parse_label::<(_, ErrorKind)>("Loop:\nMore Stuff"),
            Ok((
                "More Stuff",
                Node::Label(operands::Label::Named("Loop".into()))
            ))
        );
    }

    #[test]
    fn test_comment() {
        assert_eq!(
            parse_comment::<(_, ErrorKind)>(";lovely message"),
            Ok(("", Node::Comment("lovely message".into())))
        );
    }

    #[test]
    fn test_nodes() {
        assert_eq!(
            parse_nodes::<(_, ErrorKind)>(
                r#"
Jmp Finish
; Nice comment, yes!
Jmp Nice
Finish:
End
            "#
            ),
            Ok((
                "",
                vec![
                    Node::Instruction(
                        Instruction::Jmp(operands::Label::Named("Finish".into())),
                        (),
                    ),
                    Node::Comment(" Nice comment, yes!".into()),
                    Node::Instruction(Instruction::Jmp(operands::Label::Named("Nice".into())), (),),
                    Node::Label(operands::Label::Named("Finish".into())),
                    Node::Instruction(Instruction::End, ()),
                ]
            ))
        );
    }
}
