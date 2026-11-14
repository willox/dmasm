//! BYOND build compatibility rules established with DreamMaker probes.
//! Thresholds were checked across 515.1637–516.1659 (excluding unreleased
//! 516.1654), with additional builds noted below.

use super::CompileError;
use crate::{ByondVersion, Instruction};

/// First 516 build; 515.1647 was the final 515 build.
pub(super) const BYOND_516: u32 = 1648;

/// Builtins introduced in 516. On 515, calls resolve as user-defined globals.
const NEW_IN_516: &[&str] = &[
    "astype",
    "bound_pixloc",
    "lerp",
    "load_ext",
    "sign",
    "values_cut_over",
    "values_cut_under",
    "values_dot",
    "values_product",
    "values_sum",
    "vector",
    "pixloc",
];

/// First build restoring the cache before resolving a list element through
/// `?.` after evaluating the RHS. Earlier builds use the RHS's cache value.
pub(super) const SAFE_ELEMENT_CACHE: u32 = 1685;

/// First build preserving null checks in logical assignments through `?.`.
/// dmasm preserves them on older builds too and emits a warning.
pub(super) const SAFE_LOGICAL_ASSIGN: u32 = 1673;

/// First build dropping the index in `*p[i] = x`. dmasm writes the element
/// and warns about the difference.
pub(super) const POINTER_INDEX_DROPPED: u32 = 1668;

/// The first build with `__PROC__`.
pub(super) const PROC_MACRO: u32 = 1644;

/// Whether `name` is a builtin proc in `byond`.
pub(super) fn has_builtin(byond: ByondVersion, name: &str) -> bool {
    byond.at_least(BYOND_516) || !NEW_IN_516.contains(&name)
}

/// Reject a feature unavailable in `byond`.
pub(super) fn require(byond: ByondVersion, first: u32, what: &str) -> Result<(), CompileError> {
    if byond.at_least(first) {
        Ok(())
    } else {
        Err(CompileError::NotInByond {
            what: what.to_owned(),
            byond,
        })
    }
}

/// `ins` as `byond`'s DreamMaker writes it: 515 has `Sin`, `Cos` and `Tan`
/// where 516 has `Sin2`, `Cos2` and `Tan2`.
pub(super) fn instruction(byond: ByondVersion, ins: Instruction) -> Instruction {
    if byond.at_least(BYOND_516) {
        return ins;
    }
    match ins {
        Instruction::Sin2 => Instruction::Sin,
        Instruction::Cos2 => Instruction::Cos,
        Instruction::Tan2 => Instruction::Tan,
        ins => ins,
    }
}

/// Whether constant isnan/isinf results can be folded normally.
/// 516.1648–1669 and 516.1671 emit `PushInt result; GetFlag`, returning
/// the preceding test flag. 516.1670 and 516.1672 onward emit a constant.
pub(super) fn clean_nan_fold(byond: ByondVersion) -> bool {
    byond.build == 1670 || byond.at_least(1672)
}

/// `isnan(x)` or `isinf(x)` of a number known when compiling.
pub(super) fn nan_test(name: &str, x: f32) -> Option<bool> {
    match name {
        "isnan" => Some(x.is_nan()),
        "isinf" => Some(x.is_infinite()),
        _ => None,
    }
}

/// The first build that folds `log(b, x)` in single precision; before it the
/// quotient of the logarithms is in double precision (and rounded once).
pub(super) const SINGLE_LOG: u32 = 1644;

/// `log(base, x)` folded as `byond`'s DreamMaker does.
pub(super) fn log(byond: ByondVersion, x: f32, base: f32) -> f32 {
    if byond.at_least(SINGLE_LOG) {
        x.ln() / base.ln()
    } else {
        ((x as f64).ln() / (base as f64).ln()) as f32
    }
}

/// First build rounding inverse-trig radians to f32 before converting
/// to degrees in f64 on Linux. Earlier builds use f64 throughout.
pub(super) const SINGLE_RADIANS: u32 = 1644;

/// An angle from an inverse trig function, in degrees, as `byond`'s
/// DreamMaker folds it on Linux.
pub(super) fn degrees(byond: ByondVersion, radians: f64) -> f64 {
    if byond.at_least(SINGLE_RADIANS) {
        radians as f32 as f64 * 180.0 / std::f64::consts::PI
    } else {
        degrees_on_windows(radians)
    }
}

/// Fold inverse-trig degrees as Windows DreamMaker does, using f64
/// throughout. Some results differ from Linux by one f32 rounding step
/// (`arctan(2)`: 63.434948 vs 63.43495). DreamDaemon uses the Linux
/// angle result on both platforms.
pub(super) fn degrees_on_windows(radians: f64) -> f64 {
    radians / (std::f64::consts::PI / 180.0)
}

/// First checked Linux build using `fold::rgb_text`'s channel rounding.
/// 515.1637 differs near half-integer boundaries; intermediate builds
/// were not probed.
pub(super) const LINUX_COLOURS: u32 = 1647;

/// Negative x86 NaN produced by invalid operations.
pub(super) const DEFAULT_NAN: f64 = f64::from_bits(0xFFF8_0000_0000_0000);

/// Fold sin/cos for the target build. 516 normalizes to [0, 360) and
/// reduces by quadrants, giving exact axis values (including cos(90) = -0).
/// 515 converts the original angle directly to radians.
pub(super) fn sin_cos(byond: ByondVersion, sin: bool, x: f64) -> f64 {
    if !byond.at_least(BYOND_516) {
        // Use x86's negative NaN for infinity, regardless of the host sin().
        if x.is_infinite() {
            return DEFAULT_NAN;
        }
        let r = x.to_radians();
        return if sin { r.sin() } else { r.cos() };
    }
    // 516 removes a negative angle's sign before the operation, then restores
    // it for sin(). Infinity produces x86's negative NaN; cos() clears the
    // sign of a NaN input.
    if !x.is_finite() {
        let nan = if x.is_nan() { x } else { DEFAULT_NAN };
        return match (sin, x < 0.0) {
            (true, true) => -nan,
            (true, false) => nan,
            (false, _) if x.is_nan() => nan.abs(),
            (false, _) => nan,
        };
    }
    let mut a = x % 360.0;
    if a < 0.0 {
        a += 360.0;
    }
    let quarter = (a / 90.0).floor();
    let r = ((a - 90.0 * quarter) * (std::f64::consts::PI / 180.0)).sin_cos();
    let (s, c) = (r.0, r.1);
    match (quarter as u32 % 4, sin) {
        (0, true) => s,
        (0, false) => c,
        (1, true) => c,
        (1, false) => -s,
        (2, true) => -s,
        (2, false) => -c,
        (_, true) => -c,
        (_, false) => s,
    }
}

/// Fold tan for the target build. 515 uses direct radian conversion.
/// 516 normalizes to (-180, 180], reduces to at most 45 degrees, computes
/// in f64, then rounds to f32 before reciprocal and sign adjustments.
/// This ordering matters: tan(90) = -inf, tan(270) = inf, and tan(60)
/// is the f32 reciprocal of tan(30).
pub(super) fn tan(byond: ByondVersion, x: f32) -> f32 {
    if !byond.at_least(BYOND_516) {
        return (x as f64).to_radians().tan() as f32;
    }
    let mut a = x as f64 % 360.0;
    if a > 180.0 {
        a -= 360.0;
    } else if a <= -180.0 {
        a += 360.0;
    }
    let negative = a < 0.0;
    let a = a.abs();
    let quarter = (a / 90.0).floor();
    let mut rest = a - 90.0 * quarter;
    let mut inverse = false;
    if rest > 45.0 {
        rest = 90.0 - rest;
        inverse = true;
    }
    let mut t = rest.to_radians().tan() as f32;
    let odd = quarter as u32 % 2 == 1;
    if odd != inverse {
        t = 1.0 / t;
    }
    if odd != negative {
        t = -t;
    }
    t
}

/// Whether the target build folds this math builtin. 516 adds folding
/// for ceil, floor, round, trunc, fract, isnan and isinf.
pub(super) fn folds(byond: ByondVersion, name: &str) -> bool {
    match name {
        "ceil" | "floor" | "round" | "trunc" | "fract" | "isnan" | "isinf" => {
            byond.at_least(BYOND_516)
        }
        _ => true,
    }
}
