mod nqcrc;
mod xorjump;

use std::{convert::TryInto};

use nom::{
    branch::alt,
    bytes::{complete::{take_while, take}, streaming::tag},
    character::{
        is_digit,
    },
    combinator::{map, map_res},
    number::complete::{le_f32, le_u16, le_u32, le_u8, le_i32},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

#[derive(Debug)]
pub struct VariableId(u32);

#[derive(Debug)]
pub struct StringId(u32);

#[derive(Debug)]
pub struct MiscId(u32);

#[derive(Debug)]
pub struct BytecodeId(MiscId);

#[derive(Debug)]
pub struct LocalsId(MiscId);

#[derive(Debug)]
pub struct ParametersId(MiscId);

#[derive(Debug)]
pub struct ProcEntry {
    path: StringId,
    name: StringId,
    bytecode: BytecodeId,
    locals: LocalsId,
    parameters: ParametersId,
}

#[derive(Debug)]
pub struct DmbFile {
    strings: Vec<DMString>,
    procs: Vec<ProcEntry>,
}

#[derive(Debug)]
struct Grid;

#[derive(Debug)]
struct Class {
    path: u32,
    parent: Option<u32>,
    name: Option<u32>,
    description: Option<u32>,
    icon: Option<u32>,
    icon_state: Option<u32>,
    direction: u8,
    interface: u32, // TODO: preserve long-ness. Do I care?
    text: Option<u32>,
    unk_0: Option<u32>,
    unk_1: u16,
    unk_2: u16,
    unk_3: u16,
    unk_4: u16,
    suffix: Option<u32>,
    flags: u32, // TODO: preserve long-ness. Do I care?
    verbs: Option<u32>,
    procs: Option<u32>,
    initializer: Option<u32>,
    initialized_vars: Option<u32>,
    defining_vars: Option<u32>,
    layer: f32,
    floats: Option<[f32; 6]>,
    more_floats: Option<[f32; 20]>,
    overriding_vars: Option<u32>,
}

#[derive(Debug)]
struct Mob {
    class: u32,
    key: Option<u32>,
    sight_flags: u8,
    sight_flags_ex: Option<u32>,
    see_in_dark: Option<u8>,
    see_invisible: Option<u8>,
}

#[derive(Debug)]
pub struct DMString {
    data: Vec<u8>,
}

#[derive(Copy, Clone, Debug)]
struct Parser<'a> {
    data: &'a [u8],
    header: Header,
}

impl<'a> Parser<'a> {
    fn new(i: &'a [u8]) -> Self {
        let data = i;

        // state-less parsing of header
        let (i, header) = header(i).unwrap();

        println!("header = {:?}", header);

        let parser = Self { data, header };

        let (i, grid) = parser.grid(i).unwrap();
        let (i, string_bytes) = parser.string_bytes(i).unwrap();
        let (i, classes) = parser.classes(i).unwrap();
        let (i, mobs) = parser.mobs(i).unwrap();

        let (i, strings) = parser.strings(i).unwrap();

        parser
    }

    // i like to live dangerously
    fn offset(&self, i: &[u8]) -> usize {
        unsafe {
            i.as_ptr()
                .offset_from(self.data.as_ptr())
                .try_into()
                .unwrap()
        }
    }

    fn string_bytes(&self, i: &'a [u8]) -> IResult<&'a [u8], u32> {
        le_u32(i)
    }

    fn object(&self, i: &'a [u8]) -> IResult<&'a [u8], u32> {
        if self.header.flags.large_object_ids {
            return le_u32(i);
        }

        map(le_u16, |x| x.try_into().unwrap())(i)
    }

    fn optional_object(&self, i: &'a [u8]) -> IResult<&'a [u8], Option<u32>> {
        let (i, object) = self.object(i)?;
        if object == 0xFFFF {
            return Ok((i, None));
        }

        Ok((i, Some(object)))
    }

    fn grid(&self, i: &'a [u8]) -> IResult<&'a [u8], Grid> {
        let (i, z_width) = le_u16(i)?;
        let (i, z_height) = le_u16(i)?;
        let (i, z_count) = le_u16(i)?;

        let mut count = z_width as u64 * z_height as u64 * z_count as u64;

        let mut i = i;
        while count != 0 {
            let inner_i = i;
            let (inner_i, _turf) = self.optional_object(inner_i)?;
            let (inner_i, _area) = self.optional_object(inner_i)?;
            let (inner_i, _additional_turfs) = self.optional_object(inner_i)?;
            let (inner_i, copies) = le_u8(inner_i)?;

            assert!(copies > 0);
            count = count.saturating_sub(copies as u64);
            i = inner_i;
        }

        Ok((i, Grid))
    }

    fn classes(&self, i: &'a [u8]) -> IResult<&'a [u8], Vec<Class>> {
        let (i, count) = self.object(i)?; // should be u32?

        println!("Loading {} classes", count);

        let mut classes = vec![];
        classes.reserve(count.try_into().unwrap());

        let mut i = i;
        for _ in 0..count {
            let (inner_i, class) = self.class(i)?;
            classes.push(class);
            i = inner_i;
        }

        Ok((i, classes))
    }

    fn class(&self, i: &'a [u8]) -> IResult<&'a [u8], Class> {
        let (i, path) = self.object(i)?;
        let (i, parent) = self.optional_object(i)?;
        let (i, name) = self.optional_object(i)?;
        let (i, description) = self.optional_object(i)?;
        let (i, icon) = self.optional_object(i)?;
        let (i, icon_state) = self.optional_object(i)?;
        let (i, direction) = le_u8(i)?;


        let (i, interface) = {
            let mut i = i;

            if self.header.major >= 307 {
                let res = le_u8(i)?;
                i = res.0;
                let mut interface = res.1 as u32;

                if interface == 0x0F {
                    let res = le_u32(i)?;
                    i = res.0;
                    interface = res.1;
                }

                (i, interface)
            } else {
                (i, 1)
            }
        };

        let (i, text) = self.optional_object(i)?;

        let (i, unk_0, unk_1, unk_2) = if self.header.rhs >= 494 {
            let (i, unk_0) = self.optional_object(i)?;
            let (i, unk_1) = le_u16(i)?;
            let (i, unk_2) = le_u16(i)?;
            (i, unk_0, unk_1, unk_2)
        } else {
            (i, None, 0, 0)
        };

        let (i, unk_3, unk_4) = if self.header.rhs >= 508 {
            let (i, unk_3) = le_u16(i)?;
            let (i, unk_4) = le_u16(i)?;
            (i, unk_3, unk_4)
        } else {
            (i, 0, 0)
        };

        let (i, suffix) = self.optional_object(i)?;

        let (i, flags) = if self.header.major >= 306 {
            if self.header.rhs >= 514 {
                let (i, _something_important) = le_u32(i)?; // TODO
                let (i, flags) = le_u32(i)?;
                (i, flags)
            } else {
                le_u32(i)?
            }
        } else {
            let (i, value) = le_u8(i)?;

            if self.header.rhs >= 0x203 { // TODO: check me
                unimplemented!()
            }

            (i, value as u32)
        };

        let (i, verbs) = self.optional_object(i)?;
        let (i, procs) = self.optional_object(i)?;
        let (i, initializer) = self.optional_object(i)?;
        let (i, initialized_vars) = self.optional_object(i)?;
        let (i, defining_vars) = self.optional_object(i)?;

        let (i, _something_else) = if self.header.rhs >= 514 {
            self.optional_object(i)?
        } else {
            (i, None)
        };

        let (i, layer) = if self.header.major >= 267 {
            le_f32(i)?
        } else {
            (i, 0.0)
        };



        let (i, floats) = if self.header.rhs >= 500 {
            let (i, has) = le_u8(i)?;

            if has != 0 {
                let mut floats: [f32; 6] = [0.0; 6];

                let mut i = i;
                for idx in 0..6 {
                    let res = le_f32(i)?;
                    i = res.0;
                    floats[idx] = res.1;
                }
                (i, Some(floats))
            } else {
                (i, None)
            }
        } else {
            (i, None)
        };

        let (i, more_floats) = if self.header.rhs >= 509 {
            let (i, has) = le_u8(i)?;

            if has != 0 {
                let mut floats: [f32; 20] = [0.0; 20];

                let mut i = i;
                for idx in 0..20 {
                    let res = le_f32(i)?;
                    i = res.0;
                    floats[idx] = res.1;
                }
                (i, Some(floats))
            } else {
                (i, None)
            }
        } else {
            (i, None)
        };

        let (i, overriding_vars) = if self.header.major >= 306 {
            let (i, overriding_vars) = self.optional_object(i)?;
            (i, overriding_vars)
        } else {
            (i, None)
        };

        Ok((
            i,
            Class {
                path,
                parent,
                name,
                description,
                icon,
                icon_state,
                direction,
                interface,
                text,
                unk_0,
                unk_1,
                unk_2,
                unk_3,
                unk_4,
                suffix,
                flags,
                verbs,
                procs,
                initializer,
                initialized_vars,
                defining_vars,
                layer,
                floats,
                more_floats,
                overriding_vars,
            },
        ))
    }

    fn mobs(&self, i: &'a [u8]) -> IResult<&'a [u8], Vec<Mob>> {
        let (i, count) = self.object(i)?;

        let mut mobs = vec![];
        mobs.reserve(count.try_into().unwrap());

        let mut i = i;
        for _ in 0..count {
            let (inner_i, mob) = self.mob(i)?;
            mobs.push(mob);
            i = inner_i;
        }

        Ok((i, mobs))
    }

    fn mob(&self, i: &'a [u8]) -> IResult<&'a [u8], Mob> {
        let (i, class) = self.object(i)?;
        let (i, key) = self.optional_object(i)?;
        let (i, sight_flags) = le_u8(i)?;

        let (i, sight_flags_ex, see_in_dark, see_invisible) = if sight_flags >= 0x80 {
            let (i, sight_flags_ex) = le_u32(i)?;
            let (i, see_in_dark) = le_u8(i)?;
            let (i, see_invisible) = le_u8(i)?;
            (
                i,
                Some(sight_flags_ex),
                Some(see_in_dark),
                Some(see_invisible),
            )
        } else {
            (i, None, None, None)
        };

        Ok((
            i,
            Mob {
                class,
                key,
                sight_flags,
                sight_flags_ex,
                see_in_dark,
                see_invisible,
            },
        ))
    }

    fn strings(&self, i: &'a [u8]) -> IResult<&'a [u8], Vec<DMString>> {
        let mut hash_state: i32 = -1;

        let (i, count) = self.object(i)?;

        println!("loading {:?} strings", count);

        let mut strings = vec![];
        strings.reserve(count.try_into().unwrap());

        let mut i = i;
        for _ in 0..count {
            let (inner_i, string) = self.string(&mut hash_state, i)?;
            strings.push(string);
            i = inner_i;
        }

        let i = if self.header.major >= 468 {
            let (i, expected_hash) = le_i32(i)?;
            if expected_hash != hash_state {
                panic!("oh noooo");
            }
            i
        } else {
            i
        };

        Ok((i, strings))
    }

    fn string(&self, hash_state: &mut i32, i: &'a [u8]) -> IResult<&'a [u8], DMString> {
        let offset = self.offset(i);

        let (i, length) = le_u16(i)?;
        let length = length ^ ((offset & 0xFFFF) as u16);

        if length == 0xFFFF {
            unimplemented!();
        }

        let offset = self.offset(i);
        let (i, data)  = take(length)(i)?;
        let data = xorjump::xorjump(offset as u8, data);

        nqcrc::hash(hash_state, &data);
        nqcrc::hash(hash_state, &[0]);

        Ok((i, DMString {
            data
        }))
    }
}

#[derive(Copy, Clone, Debug)]
struct Header {
    major: u32,
    lhs: u32,
    rhs: u32,
    flags: Flags,
}

#[derive(Copy, Clone, Debug)]
struct Flags {
    large_object_ids: bool,
}

/// parses until a non-numeric character is hit, we might be using this in places that we shouldn't
fn parse_plaintext_uint(i: &[u8]) -> IResult<&[u8], u32> {
    map_res(take_while(is_digit), |x: &[u8]| {
        let string = std::str::from_utf8(x).unwrap();
        string.parse::<u32>()
    })(i)
}

// NOTE: older versions of byond have a different format here
fn header(i: &[u8]) -> IResult<&[u8], Header> {
    // TODO: shebang mess
    map(
        tuple((
            delimited(tag("world bin v"), parse_plaintext_uint, tag("\x0A")),
            delimited(tag("min compatibility v"), parse_plaintext_uint, tag(" ")),
            terminated(parse_plaintext_uint, tag("\n")),
            header_flags,
        )),
        |(major, lhs, rhs, flags)| Header {
            major,
            lhs,
            rhs,
            flags,
        },
    )(i)
}

fn header_flags(i: &[u8]) -> IResult<&[u8], Flags> {
    alt((
        map_res(le_u32, |flags| {
            if (flags & (1 << 31)) != 0 {
                return Err(());
            }

            Ok(Flags {
                large_object_ids: (flags & (1 << 30) != 0),
            })
        }),
        map(preceded(le_u32, le_u32), |flags| Flags {
            large_object_ids: (flags & (1 << 30) != 0),
        }),
    ))(i)
}

#[cfg(test)]
mod tests {
    // const EXAMPLE_DMB: &'static [u8] =
    //     include_bytes!("E:\\spantest_char_crash\\spantest_char_crash.dmb");
    const EXAMPLE_DMB: &'static [u8] = include_bytes!("E:\\tgstation\\tgstation.dmb");

    #[test]
    fn it_works() {
        let parser = super::Parser::new(EXAMPLE_DMB);
    }
}
