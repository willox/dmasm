macro_rules! access_modifiers {
	( $( $name:ident = $value:tt ),* $(,)? ) => {
        $(
            #[allow(non_upper_case_globals)]
            pub const $name: u32 = $value;
        )*
	}
}

access_modifiers! {
    Usr = 0xFFCD,
    Src = 0xFFCE,
    Args = 0xFFCF,
    Dot = 0xFFD0,
    // UnkFFD1 = 0xFFD1,
    // UnkFFD2 = 0xFFD2,
    // UnkFFD3 = 0xFFD3,
    // UnkFFD4 = 0xFFD4,
    // UnkFFD5 = 0xFFD5,
    // UnkFFD6 = 0xFFD6,
    // UnkFFD7 = 0xFFD7,
    Cache = 0xFFD8,
    Arg = 0xFFD9,
    Local = 0xFFDA,
    Global = 0xFFDB,
    Field = 0xFFDC,
    SrcProc2 = 0xFFDD,
    SrcProc = 0xFFDE,
    Proc = 0xFFDF,
    Proc2 = 0xFFE0,
    // UnkFFE1 = 0xFFE1,
    // UnkFFE2 = 0xFFE2,
    Cache2 = 0xFFE3,
    Cache3 = 0xFFE4,
    World = 0xFFE5,
    Null = 0xFFE6,
    Initial = 0xFFE7,
    // UnkFFE8 = 0xFFE8,
    // UnkFFE9 = 0xFFE9,
    // UnkFFEA = 0xFFEA,
    // UnkFFEB = 0xFFEB,
    // UnkFFEC = 0xFFEC,
}

pub fn is_access_modifier(value: u32) -> bool {
    value >= 0xFFCD && value <= 0xFFEC
}
