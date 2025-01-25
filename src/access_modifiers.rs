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
    PtrDeref = 0xFFD5,
    // UnkFFD6 = 0xFFD6,
    // UnkFFD7 = 0xFFD7,
    Cache = 0xFFD8,
    Arg = 0xFFD9,
    Local = 0xFFDA,
    Global = 0xFFDB,
    SetCache = 0xFFDC,
    DynamicProc = 0xFFDD,
    DynamicVerb = 0xFFDE,
    StaticProc = 0xFFDF,
    StaticVerb = 0xFFE0,
    // UnkFFE1 = 0xFFE1,
    // UnkFFE2 = 0xFFE2,

    // These are used for r-value list accessors
    // Cache2 = list key
    // Cache3 = reference to Cache[Cache2]
    CacheKey = 0xFFE3,
    CacheIndex = 0xFFE4,

    World = 0xFFE5,
    Null = 0xFFE6,
    Initial = 0xFFE7,
    IsSaved = 0xFFE8,
    // UnkFFE9 = 0xFFE9,
    // UnkFFEA = 0xFFEA,
    // UnkFFEB = 0xFFEB,
    // UnkFFEC = 0xFFEC,
    // UnkFFED = 0xFFED,
    PtrRef = 0xFFEF,
}

pub fn is_access_modifier(value: u32) -> bool {
    (0xFFCD..=0xFFEF).contains(&value)
}
