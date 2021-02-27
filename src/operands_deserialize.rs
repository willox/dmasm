use crate::list_operands::*;
use crate::operands::{OperandDeserialize, *};
use crate::parser;
use nom::combinator::*;
use nom::error::FromExternalError;
use nom::error::ParseError;
use nom::sequence::*;
use nom::{character::complete::*, *};

impl OperandDeserialize for u32 {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // TODO: hex/binary literals
        map_res(digit1, |x: &str| x.parse::<u32>())(i)
    }
}

impl OperandDeserialize for i32 {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // TODO: hex/binary literals
        map_res(recognize(tuple((opt(one_of("+-")), digit1))), |x: &str| {
            x.parse::<i32>()
        })(i)
    }
}

impl OperandDeserialize for Label {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(parser::parse_identifier, |x: &str| Label(x.into()))(i)
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
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // TODO: String formatting
        // map(
        //     delimited(char('"'), recognize(take_while(|x| x != '"')), char('"')),
        //     |x: &str| DMString(x.into()),
        // )(i)
        unimplemented!()
    }
}

impl OperandDeserialize for RangeParams {
    fn deserialize<'a, E>(i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        // It's nothing! This works, right?
        Ok((i, RangeParams))
    }
}

impl OperandDeserialize for IsInParams {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}

impl OperandDeserialize for SwitchParams {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}

impl OperandDeserialize for PickSwitchParams {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}

impl OperandDeserialize for SwitchRangeParams {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}

impl OperandDeserialize for PickProbParams {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}

impl OperandDeserialize for Value {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
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

impl OperandDeserialize for TypeFilter {
    fn deserialize<'a, E>(_i: &'a str) -> IResult<&str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        panic!("TODO");
    }
}
