# Proc compiler

`dmasm::compiler::compile_proc` compiles a proc parsed by SpacemanDMM's
`dreammaker` crate into nodes that assemble to BYOND bytecode. It targets the
running world's BYOND build so the result can replace a proc through hot reload.

## Validation

The comparison harness compiles every proc in a codebase, assembles it against
DreamMaker's `.dmb`, and compares the resulting disassembly. Results from 2026-11-10:

| Codebase commit | BYOND | Procs | Identical | Equivalent | Known differences |
| --- | --- | --- | --- | --- | --- |
| cmss13 `f712f7b` | 516.1687 | 28,330 | 19,456 | 8,872 | 2 |
| tgstation `928bb812` | 516.1685 | 66,712 | 43,255 | 23,422 | 35 |
| goonstation `3e27edf` | 516.1687 | 44,522 | 29,643 | 14,875 | 2 |
| tgstation `addb78ce0b` | 516.1659 | 65,478 | 42,499 | 22,944 | 35 |
| tgstation `6a18424329` | 515.1647 | 58,937 | 38,860 | 20,041 | 36 |
| tgstation `7faadfdc16` | 515.1637 | 58,171 | 38,343 | 19,791 | 37 |

“Identical” includes debug information. “Equivalent” excludes debug information
and applies only the encoding normalizations below. The remaining mismatches are
known differences described here.

The harness also replaces proc bytecode in the `.dmb` and runs codebase unit tests:
150 for cmss13, 486 for tgstation and 22 for goonstation, with the same results as
DreamMaker builds. Differential fuzzing compared 1,536 worlds of 80 generated procs.
The offline `.dmb` tooling is not included in this repository.

## Target BYOND build

Set `ProcSource::byond`, `compile_expr_for` or `reload::ParseOptions::byond` to the
running world's build. `ByondVersion::read_as` configures version-specific parsing.
Compatibility rules and thresholds are in `src/compiler/versions.rs`.

Probes covered 515.1637, 515.1647, 516.1659, 516.1685 and 516.1688. Additional probes
located behavior changes across 515.1637–516.1660, and in 516.1664, 1668–1672, 1676,
1681 and 1684.

- **Builtins and syntax:** 515 uses `Sin`, `Cos`, `Tan` rather than their `*2`
  instructions. Calls to unavailable builtins resolve as global procs. Unsupported
  `alist()` and key/value iteration require 516. `__PROC__` requires 515.1644.
- **Integer literals:** before 515.1644, integer literals above 2147483647 clamp to
  that value, including hexadecimal and octal literals. Decimal-point and exponent
  forms such as `3000000000.0` and `3e9` are unaffected.
- **Folding:** 515 does not fold `ceil`, `floor`, `round`, `trunc` or `fract`.
  516 also folds `isnan` and `isinf`, with the flag-register quirk below.

## Constant folding and platform differences

The compiler follows Linux DreamMaker. It has no build-platform option.

### Math

515 converts trigonometric inputs directly to radians. 516 normalizes angles and
reduces by quadrants, producing exact axis values for sin/cos. For tan, it reduces
to at most 45 degrees, computes in f64, rounds to f32, then applies reciprocal and
sign adjustments. These steps affect the result and must retain their order.
`arctan(x, -0)` treats the second argument as zero. `%%` folds with f64 `fmod`.

From 515.1644, Linux DreamMaker rounds inverse-trig radians to f32 before converting
to degrees. Windows uses f64 throughout. Some constants differ by one f32 rounding
step (`arctan(2)`: 63.43495 on Linux, 63.434948 on Windows). DreamDaemon uses the
Linux angle result on both platforms. The compiler warns when Windows folding differs.

### Colors

`rgb()` folds in every color space. RGB and alpha channels truncate to integers;
HSV, HSL and HCY channels round half up. HSL clamps saturation and luminance to
0–100; HSV clamps from 516; HCY does not clamp. HCY uses a green weight of 0.557
for `rgb()` and 0.587 for `rgb2num()`.

Linux DreamMaker from 515.1647 normalizes percentage inputs in f32, computes in f64,
then rounds channels through f32 before integer rounding. Windows and 515.1637 use
different intermediate precision. Channels near a half-integer boundary can differ
by one (276 of 31,752 sampled colors differed between Linux and Windows on
516.1688). The compiler uses Linux rounding
and warns near those boundaries. Before 515.1647, it leaves such colors for runtime.

`rgb2num()` folds RGB results to integer lists. Inputs include short and long hex
colors with optional alpha, 22 case-insensitive color names, and null (white).
Spaces other than 1, 2 and 3 select RGB. In a condition, the result is known to be
true in any color space without constructing the list. HSV/HSL/HCY results remain runtime expressions
because their folded precision differs between builds and platforms.

Runtime color conversion follows the local DreamDaemon platform. Runtime angle
conversion follows Linux on both platforms. Other platform differences found in
numeric, text, statement and builtin sweeps were limited to out-of-range bit operands.

## Equivalent encodings

The comparison normalizes:

- Static versus dynamic proc calls. DreamMaker chooses based on compilation order;
  dmasm uses dynamic calls because that order is unknown.
- Label names and switch target encodings, comparing resolved destinations instead.
- Redundant cache loads of plain `src`, arguments or locals.

Cache reuse through fields is behaviorally significant and is not normalized.
For example, after `a.b.f()` changes `a.b`, DreamMaker still uses the cached old
receiver for `a.b.x`. dmasm matches these reuse and invalidation rules.

## Known differences

| Source or case | DreamMaker | dmasm |
| --- | --- | --- |
| Parameter default `a.b/10` without spaces | Parses a path and omits division; `x/-10` becomes subtraction | Performs division |
| `a?.f(b?.x)?.y = v` | Calls f on the receiver left by argument evaluation | Restores f's receiver |
| `a?.b \|\|= v` or `&&=` before 516.1673 | Omits null checks; with multiple `?.` accesses, reads and writes the first object's variable | Preserves null checks and the final target |
| `/T/name` subtype block where name is a proc of T | Appends the block to the proc | Parses a subtype |
| Empty proc at a file's end | Names the next included file in debug info | Uses the proc's source file |
| `*p[i] = x` from 516.1668 | Drops the index and writes through the cache | Writes the indexed element |
| Out-of-range constants in `&`, `\|`, `^`, `<<`, `>>` | Inconsistent, build-dependent 24-bit conversion | Uses the low 24 bits of the nearest 32-bit integer |
| `pick()` ending in a zero-weight entry | Jumps outside the proc if no earlier entry is selected | Selects the last entry whose weight is not a constant zero |

Hexadecimal literals longer than eight digits and octal literals exceeding i32
are rejected. DreamMaker's conversion is unusual (`0x100000000` becomes `2**60`)
and is not implemented.

## Warnings

`CompiledProc::warnings` reports intentional behavior differences and significant
DreamMaker quirks retained for compatibility. Reload forwards these diagnostics.

Warnings cover parameter-default division, safe-call receivers, logical assignment
before 516.1673, indexed pointer assignment from 516.1668, out-of-range bit operands,
zero-weight `pick()` endings, and platform-sensitive angle/color folding.

Two copied quirks also warn:

- In 516.1648–516.1669 and 516.1671, constant `isnan`/`isinf` tests emit a result
  followed by `GetFlag`, returning the previous test flag.
- Before 516.1685, changing a list element through `?.` with an RHS that uses
  the cache resolves the list from the cache value left by the RHS.

After `a.b.f()`, DreamMaker keeps `a.b` cached: a later `a.b.x` in the same
flow uses the old object even if `f()` replaced `a.b`. dmasm preserves this
behavior without warning because the pattern is common and only matters when
the call replaces the field. Read the field into a local when a call may replace it.

Debug-location and encoding-only differences do not warn. In the validation corpora,
most warnings are tgstation parameter-default divisions; older builds also expose
the safe-list-assignment quirk.

## Compiler compatibility rules

These rules affect generated code and are covered by focused tests:

- User const variables, including local consts, occupy global slots. Standalone
  reads access the slot; larger constant expressions fold the value. Built-in
  constants such as `NORTH` are emitted directly.
- Static locals are block-scoped. Scope `proc:PATH#N` identifies the Nth declaration
  of a given name in that proc.
- Static/const access through `x::a::b` does not evaluate receivers preceding b.
  `/type::proc()` selects that type's override. `:` lookup searches subtypes depth
  first; unknown receiver calls use the first declaration's display name.
- After the first for loop, label, switch or pick(), uninitialized variable
  declarations explicitly assign null.
- For loops, ternary else branches and discarded logical operands invalidate cache
  knowledge. Labels retain it only with one incoming edge; switch cases sharing a
  target count as one, while fallthrough after `return` still counts.
- Assignment statements invalidate cached field chains, except list-entry writes,
  most declarations and assignments compiled as values. `var/L[] = new()` is an
  exception among declarations. Calls outside the cache invalidate chains; receiver
  calls, `..()` and spawn preserve them. `x.y ||= v` resolves its target before testing.
- Logical assignments resolve fields/list entries once, saving the cache across
  RHS code that needs it. Static/const reads through `.` and `global.x` do not
  require a save; `::` accesses do. A list-indexing RHS also saves the target key.
- `del(x)` clears a writable target before deletion; `del src` ends the proc.
  `del(world)` only deletes.
- Empty arguments use `PushVal null`; explicit null uses `GetVar null`.
  A `null = x` associative key becomes the text `"null"`.
- `var/L[n]` emits `n; EmptyList`; multidimensional declarations use `new /list`.
  In `var/x = a in L`, L is the declaration's input clause. Statement `x = a in L`
  parses as `(x = a) in L`.
- In return values, arguments and parentheses, regroup associative `+ * & | ^`
  expressions before folding. Fold math builtins except min/max; reversed clamp
  bounds are swapped. `X * X` uses PowSquare for supported values, excluding
  interpolated strings and fractions.
- Binary bit operations clamp operands to i32 and retain 24 bits. `~` converts
  through u32, with negative and overflow behavior covered by tests.
- Safe assignment emits nested null checks across fields, calls and indexes.
  Value expressions use SetCacheJmpIfNull; statement targets use SetCachePopJmpIfNull.
- PickSwitch always jumps, preserving cache knowledge for its first arm.
- Spawn without a block applies to the next statement. Its code retains enclosing
  loop targets. Labeled break exits the block; try-block exits emit Catch/TryJmp.
- `for (var/datum/T/x)` enumerates all datums of type T. With `in world`, it
  enumerates atoms and checks their type, so finds no datums.
  `for (var/list/T/x in L)` checks x as a T.
- `for (var/T/k, v in L)` filters and type-checks the key as in
  `for (var/T/x in L)`.
- Input has four arguments, padded with null, and a built-in or stack-based `in`
  source. `findlasttext` defaults to start 0 and end 1.
- Absolute-path parameters and `...` do not allocate parameter slots.
- Prefabs use instance-table entries with init procs. Order variable assignments
  by string ID and emit source locations before computed values. Assembly resolves
  the instance through `AssembleEnv::get_instance`.

## Unsupported forms

Compilation returns an error for unsupported constructs, including savefile-to-
savefile reads (`S["a"] >> S["b"]`), standalone `load_resource()` and nonconstant
switch cases. The caller must retain existing bytecode or reject the reload.

Assembly also needs resources, strings, procs and prefab instances available through
its environment. Resources must already exist in the world's `.rsc`; adding runtime
table entries is the hot reload caller's responsibility.

## Parser dependency

`Cargo.toml` uses [auxtools/SpacemanDMM](https://github.com/auxtools/SpacemanDMM)
for parser and preprocessor fixes required by compilation: distinct alist terms,
empty arguments, for-in type clauses, macro stringification, source-location macros,
raw strings, declaration flags, pragma handling and absolute parameter metadata.
