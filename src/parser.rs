use crate::operands;
use crate::Instruction;
use crate::Node;
use nom::branch::*;
use nom::combinator::*;
use nom::multi::*;
use nom::error::FromExternalError;
use nom::error::ParseError;
use nom::sequence::*;
use nom::{character::complete::*, *};
use nom::bytes::complete::take_while;

fn parse_sint<'a, E>(i: &'a str) -> IResult<&str, i32, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    map_res(
        delimited(
            space0,
            recognize(tuple((opt(one_of("+-")), digit1))),
            space0,
        ),
        |x: &str| x.parse::<i32>(),
    )(i)
}

fn parse_uint<'a, E>(i: &'a str) -> IResult<&str, u32, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    map_res(delimited(space0, digit1, space0), |x: &str| {
        x.parse::<u32>()
    })(i)
}

fn parse_label<'a, E>(i: &'a str) -> IResult<&str, Node, E>
where
    E: ParseError<&'a str>,
{
    map(
        delimited(space0, alpha1, pair(char(':'), alt((line_ending, eof)))),
        |x: &str| Node::Label(x.into()),
    )(i)
}

fn parse_comment<'a, E>(i: &'a str) -> IResult<&str, Node, E>
where
    E: ParseError<&'a str>,
{
    map(
        preceded(
            char(';'),
            take_while(|x| x != '\r' && x != '\n'),
        ),
        |x: &str| Node::Comment(x.into())
    )(i)
}

fn parse_label_operand<'a, E>(i: &'a str) -> IResult<&str, operands::Label, E>
where
    E: ParseError<&'a str>,
{
    map(
        delimited(space0, alpha1, space0),
        |x: &str| operands::Label(x.into()),
    )(i)
}


fn parse_instruction<'a, E>(i: &'a str) -> IResult<&str, Node, E>
where
    E: ParseError<&'a str>,
{
    let (i, name) = delimited(space0, recognize(tuple((alpha1, alphanumeric0))), space0)(i)?;

    let (i, instruction) = match name {
        "End" => (i, Instruction::End),

        "Jmp" => {
            let (i, label) = parse_label_operand(i)?;
            (i, Instruction::Jmp(label))
        }

        _ => return Err(Err::Error(error_position!(i, error::ErrorKind::TagBits))),
    };

    Ok((i, Node::Instruction(instruction)))
}

fn parse_nodes<'a, E>(i: &'a str) -> IResult<&str, Vec<Node>, E>
where
    E: ParseError<&'a str>,
{
    many0(delimited(multispace0, alt((parse_label, parse_comment, parse_instruction)), multispace0))(i)
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
    fn test_uint() {
        assert_eq!(parse_uint::<(_, ErrorKind)>("1337"), Ok(("", 1337)));

        assert_eq!(parse_uint::<(_, ErrorKind)>(" 1337 "), Ok(("", 1337)));

        assert_eq!(
            parse_uint::<(_, ErrorKind)>("Hello"),
            Err(Err::Error(("Hello", ErrorKind::Digit)))
        );
    }

    #[test]
    fn test_sint() {
        assert_eq!(parse_sint::<(_, ErrorKind)>("+1337"), Ok(("", 1337)));

        assert_eq!(parse_sint::<(_, ErrorKind)>(" -1337 "), Ok(("", -1337)));

        assert_eq!(
            parse_sint::<(_, ErrorKind)>("Hello"),
            Err(Err::Error(("Hello", ErrorKind::Digit)))
        );
    }

    #[test]
    fn test_label() {
        assert_eq!(
            parse_label::<(_, ErrorKind)>("Invalid"),
            Err(Err::Error(("", ErrorKind::Char)))
        );
        assert_eq!(
            parse_label::<(_, ErrorKind)>("Loop:"),
            Ok(("", Node::Label("Loop".into())))
        );
        assert_eq!(
            parse_label::<(_, ErrorKind)>("Loop:\nMore Stuff"),
            Ok(("More Stuff", Node::Label("Loop".into())))
        );
    }

    #[test]
    fn test_label_operand() {
        assert_eq!(
            parse_label_operand::<(_, ErrorKind)>("identifier"),
            Ok(("", operands::Label("identifier".into())))
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
    fn test_instruction() {
        assert_eq!(
            parse_instruction::<(_, ErrorKind)>("Jmp Loop"),
            Ok(("", Node::Instruction(Instruction::Jmp(operands::Label("Loop".into())))))
        );
    }

    #[test]
    fn test_nodes() {
        assert_eq!(
            parse_nodes::<(_, ErrorKind)>(r#"
Jmp Finish
; Nice comment, yes!
Jmp Nice
Finish:
End
            "#),
            Ok(("", vec![
                Node::Instruction(Instruction::Jmp(operands::Label("Finish".into()))),
                Node::Comment(" Nice comment, yes!".into()),
                Node::Instruction(Instruction::Jmp(operands::Label("Nice".into()))),
                Node::Label("Finish".into()),
                Node::Instruction(Instruction::End),
            ]))
        );
    }
}
