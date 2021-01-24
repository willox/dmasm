use crate::operands::{OperandDeserialize, *};
use nom::branch::*;
use nom::bytes::complete::take_while;
use nom::combinator::*;
use nom::error::FromExternalError;
use nom::error::ParseError;
use nom::multi::*;
use nom::sequence::*;
use nom::{character::complete::*, *};

fn parse_label_identifier<'a, E>(i: &'a str) -> IResult<&str, &str, E>
where
    E: ParseError<&'a str>,
{
    recognize(pair(
        alt((alpha1, bytes::complete::tag("_"))),
        many0(alt((alphanumeric1, bytes::complete::tag("_")))),
    ))(i)
}

//

impl OperandDeserialize for u32 {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // TODO: hex/binary literals
        map_res(delimited(space0, digit1, space0), |x: &str| {
            x.parse::<u32>()
        })(i)
    }
}

impl OperandDeserialize for i32 {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // TODO: hex/binary literals
        map_res(
            delimited(
                space0,
                recognize(tuple((opt(one_of("+-")), digit1))),
                space0,
            ),
            |x: &str| x.parse::<i32>(),
        )(i)
    }
}

impl OperandDeserialize for Label {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            delimited(space0, parse_label_identifier, space0),
            |x: &str| Label(x.into()),
        )(i)
    }
}

impl OperandDeserialize for Proc {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}

impl OperandDeserialize for DMString {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // TODO: String formatting
        map(
            delimited(char('"'), recognize(take_while(|x| x != '"')), char('"')),
            |x: &str| DMString(x.into()),
        )(i)
    }
}

impl OperandDeserialize for Variable {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}
