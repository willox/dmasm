# The proc compiler

`dmasm::compiler::compile_proc` turns a proc's source, as parsed by SpacemanDMM's
`dreammaker` crate, into dmasm nodes that assemble to BYOND bytecode. The goal is code
that behaves like what DreamMaker (BYOND's compiler) produces for the same proc, so a
running world can have procs replaced with recompiled ones (hot reloading).

## How it is checked

- **Comparison with DreamMaker.** Every proc of a codebase is compiled and assembled
  against the `.dmb` DreamMaker built from the same source, disassembled, and compared
  with DreamMaker's code. On 2026-11-08, with BYOND 516.1685/1687 builds:

  | Codebase | Procs | Identical | Equivalent | Known differences |
  |---|---|---|---|---|
  | cmss13 (f712f7b) | 28,330 | 19,192 | 9,136 | 2 |
  | tgstation (928bb812) | 66,712 | 41,848 | 24,829 | 35 |
  | goonstation (3e27edf) | 44,522 | 29,083 | 15,437 | 2 |

  Every proc is identical, equivalent, or one of the known differences below.
  "Equivalent" means equal after the normalisations listed below, which only change
  encodings, not behaviour. Debug info (DbgLine, DbgFile) is left out of the comparison:
  the compiler writes its own.

- **Running both.** The procs that compile are swapped into the `.dmb` and the codebase's
  own unit tests run on the result (all the same as DreamMaker's build: cmss13 150,
  tgstation 486, goonstation 22). A fuzzer generates random procs and compares their
  results in a DreamMaker-built and a swapped world (1,536 worlds of 80 procs).

The tooling for these checks (it reads `.dmb` files offline) isn't in this repository
yet.

## Encodings that differ from DreamMaker

These are accepted as equivalent by the comparison:

- **Proc calls.** DreamMaker calls a proc it has already compiled with `static_proc`, and
  one it hasn't yet by name (`dynamic_proc`). The order it compiles procs in isn't known
  here, so calls always go by name.
- **The cache register.** DreamMaker leaves out `cache = x` when it knows the cache
  already holds `x`, and the compiler copies its rules exactly, because they change
  behaviour: after `a.b.f()` replaced `a.b`, DreamMaker's following `a.b.x` still reads
  the old object (tgstation's tests depend on it), and reading the new one breaks them;
  the other way round broke another test. The comparison counts reusing a chain through
  fields where DreamMaker loads it, or loading it where DreamMaker reuses it, as a
  mismatch; reusing or reloading a plain `src`, argument or local is equivalent.
- **Switch cases and labels** compare by where they jump, not by label names.

## Known differences in behaviour

- **Parameter defaults with `/number`.** In a parameter's default value DreamMaker reads
  `x/10` (no spaces) as a path and drops the division: `proc/f(t = (SS.wait/10))`
  defaults `t` to `SS.wait`, and `x/-10` becomes `x - 10`. The compiler does the
  division the source asks for. (tgstation's `/mob/living/proc/Life` is one such proc.)
- **`a?.f(b?.x)?.y = v`.** DreamMaker calls `f` on whatever the argument left in the
  cache; the compiler restores the receiver first.
- **`/T/name` blocks where `name` is a proc of T.** Written as a subtype with var
  lines, DreamMaker appends the lines to the proc's code instead (cmss13's
  `/obj/structure/mineral_door/wood/open`); SpacemanDMM makes the subtype the code means.
- **Empty procs at the end of a file.** DreamMaker's debug info names the next file
  included as theirs (debug info only).

## How DreamMaker compiles things (that the compiler copies)

Rules found by probing DreamMaker, which are easy to get wrong:

- A `const` var is a global: `x.CONST`, `x::CONST` and `/type::CONST` read
  `global("CONST")`, and so does a lone const name; only inside a larger constant
  expression (`CONST + 1`) is its value folded in. Builtin constants (`NORTH`) are values.
  A local `var/const` is a global of its proc too (like a static local).
- Static locals are block-scoped: two `var/static/x` in one proc are two globals. The
  assembler gets them as scope `proc:/path/of/proc#N`, the Nth distinct global of that
  name in the proc's existing code.
- `x::a::b`, where `b` is static or const, evaluates nothing before `b`. `/type::proc()`
  is the type's own override if it has one. `x:name` and `x:f()` find the var or proc on
  a subtype too, and `f()` called on a value of unknown type is called by the display
  name of the first proc or verb declared with that name.
- After a proc's first `for` loop, label, `switch` or `pick()`, every `var/x` declared
  without a value is set to null where it's declared (inside any loop as well).
- The cache: every `for` loop and the else branch of `? :` start not knowing what it
  holds, and a label knows it only with one way in (a switch's cases to one label are one
  way in; code after `return` still counts as falling through). A constant dropped from
  `&&`/`||` (`0 || x`) forgets it too.
- A chain through fields (`a.b`) in the cache is forgotten:
  - after an assignment statement (`x = y`, `x += y`, `x++`, `(x |= y)`, even to a
    local), but not after a list element's, a var declaration's (except
    `var/L[] = new()`) or a `(x = y)` that DreamMaker compiles as a value
  - after a call that isn't through the cache (`f()`, `call()()`, `del`), but not after
    `a.b.f()`, `..()` or `spawn`
- `x.y ||= v` resolves its write against the cache before the test.
- `del(x)` sets `x` (a variable, field or list element) to null before deleting, and
  `del src` ends the proc. `del(world)` just deletes.
- An empty argument, `f(a, , b)`, is `PushVal null`; the literal `null` is `GetVar null`.
  A `null = x` key is `"null"`.
- `var/L[n]` is `n; EmptyList`; `var/L[a][b]` is `new /list(a, b)`.
- `var/x = a in list(...)` is `var/x = a` (the list is x's `in` clause), and the statement
  `x = a in L` is `(x = a) in L`.
- `a + (b + c) + d` is `a + ((b + c) + d)` in return values, arguments and parentheses
  (and everything under them), for `+ * & | ^`; constants in the group then fold.
- Constants fold, including math builtins (`sqrt`, `log`, `round`, `clamp`, ...; not
  `min`/`max`) and paths in `&&`/`||`. `clamp()` with its bounds the wrong way round
  swaps them.
- `X * X` of variables, text, null, paths and arithmetic of them is `PowSquare` (not of
  embedded text, or fractions).
- Bit operations fold on 24 bits: `& | ^ << >>` clamp to the i32 range first (`2**33 & x`
  is all ones), and `~` takes its operand as a u32 (negative is 0, over 2**32 all ones,
  2**32 itself 0).
- `?.` writes: `a?.b?.c = v` nests a `SetCachePopJmpIfNull` per `?`, through calls and
  list elements (`a?.f()?.x`, `L?[k]`); as a value it uses `SetCacheJmpIfNull`.
- `x ||= y` and `x &&= y` on a field or list element find it once and keep it in the
  cache, saving the cache around `y` when `y` uses it, reads any field (even a static or
  `global.x`) or calls a global proc; a list element's key is saved too when `y` indexes a
  list.
- A `pick()`'s first case starts knowing what the cache holds (`PickSwitch` always jumps).
- `spawn` without a block spawns only the next statement; the spawned code is still in its
  loops (`continue` goes on with the loop in the new thread). `break` out of a labeled
  block jumps past it. Inside `try`, `break`/`continue` are `Catch`/`TryJmp`.
- `for (var/datum/T/x)` goes over all datums of type T; with `in world` it goes over
  the world's atoms and checks their type (so finds no datums). `for (var/list/T/x in L)`
  checks x as a T.
- `for (var/T/k, v in L)` filters and type-checks the key like `for (var/T/x in L)`.
- `input()` takes four arguments (nulls added), and its `in` list is either built in
  (`view`, `oview`, `range`, `orange` with a constant distance, `usr`, `world`) or a list on
  the stack.
- `sin`, `cos` and `tan` are `Sin2`, `Cos2` and `Tan2`; `findlasttext` defaults to
  `start = 0, end = 1`.
- A parameter written as an absolute path (`proc/f(/obj/x)`) or `...` isn't a parameter.
- Prefabs with var overrides (`/obj{a = 1}`) are instances in the `.dmb`'s instance
  table, each with an init proc (`src.a = 1`, ordered by the names' string ids, with a
  DbgLine/DbgFile before a computed value like `matrix()`). `Value::Prefab` carries the
  var code; the assembler asks `AssembleEnv::get_instance` for the matching instance.

## Not supported

The compiler returns an error for these, so such procs keep their DreamMaker code:

- A savefile element read into another (`S["a"] >> S["b"]`).
- `load_resource()` outside `x << load_resource(...)`, `switch` cases that aren't
  constants, and a few other rare forms.
- Resources (files in `'quotes'`) the `.rsc` doesn't already have, strings, procs or
  prefab instances the `.dmb` doesn't have (the hot reloader will need to add those).

## Parser fixes it depends on

Until they are in a SpacemanDMM release, `Cargo.toml` patches `dreammaker` with a local
checkout at `../SpacemanDMM` that has these. `patches/spacemandmm.patch` holds them as a
`git am` series on SpaceManiac/SpacemanDMM `a388985` ("Bump versions for suite 1.12"), and
`scripts/setup-spacemandmm.sh` clones SpacemanDMM there and applies it.

- `Term::Alist` for `alist(...)` (it parsed as `list(...)`).
- Empty arguments, `f(a, , b)`, kept as nulls (they were dropped, shifting the rest).
- The `as` clause of `for (var/x as T in L)` (it was dropped).
- Macro stringification spacing, `__LINE__` in a macro call and in a macro's body (the
  line of the `)` plus the line breaks inside the arguments before it), `EXCEPTION()`'s
  file and line, raw strings, var flags anywhere in a path, `#pragma` taking the rest of
  its line, and `Parameter::absolute`.
