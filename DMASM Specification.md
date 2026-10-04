# DMASM and the DM bytecode instruction set

`dmasm` turns a BYOND proc's bytecode into named instructions and back. This
file lists every instruction it decodes, what each one takes off the operand
stack, what it leaves there, and what it does.

Everything here describes BYOND 516, checked on 516.1687 unless a row says
otherwise, as of 2026-10-04.

Three things to know before trusting a row:

- **The instruction list and operand shapes come from `src/instructions.rs`.**
  That file wins over this one. Its operand counts were compared against the
  interpreter's own handlers on eleven Windows builds, 516.1659 to 516.1688, on
  2026-10-04, and they agree. That comparison could not check `Input` and
  `InputColor`, and it cannot see a `Variable` operand at all, so a missing
  `Variable` would pass it.
- **Behavior rows are summaries**, and the BYOND binary wins over them.
- **Every row says how it is known**, in its last column. About a quarter of the
  rows are marked `dm`: their stack effect comes from what the compiler emits
  around them, and nobody has read the handler.

The instruction names are `dmasm`'s, not BYOND's. Some are guesses and two are
known to be wrong (`Log10`, `LocateRef`); the rows say so.

## Bytecode shape

- A proc's bytecode is an array of 32-bit words. Offsets count words, not bytes.
- An instruction is one opcode word followed by zero or more operand words.
- BYOND's opcodes run `0x000..=0x18C`, 397 slots. `dmasm` decodes 387 of them.
  The other 10 are listed under "Opcode slots dmasm does not decode".
- Jump targets are absolute word offsets inside the same proc. The interpreter
  keeps its position in a 16-bit field and reads a jump operand as 16 bits.

## VM state

What an instruction can read or write.

| State | What it is | Written by | Read by |
|---|---|---|---|
| Operand stack | Array of 8-byte Values (tag, data). The first ten live in a buffer inside the proc's frame, then it moves to the heap | most instructions | most instructions |
| Test flag | One byte, true or false | `Test`, `IsLoc`/`IsMob`/`IsObj`/`IsArea`/`IsTurf`/`IsMovable`, `IsNaN`, `IsInf`, `IsIn`, `IterNext`, `TestEquiv`, `TestNotEquiv` | `Jz`, `Jnz`, `JzLoop`, `JnzLoop`, `GetFlag` |
| `dot` | The proc's `.` variable, its default return value | `SetVar dot`, compound assignment on `dot` | `End`, `GetVar dot` |
| `cache` | One Value: the object or list whose member is being accessed | the `SetCache` variable form, `SetCacheJmpIfNull`, `SetCachePopJmpIfNull`, `PopCache` | field reads and writes, calls by name |
| `cache_key` | One Value: the index into `cache` | `SetVar cache_key`, `PopCacheKey` | the `cache[cache_key]` variable form |
| `src`, `usr`, `args`, locals | The frame's own values | `SetVar` and friends | `GetVar` and friends |
| Active iterator | Array, length, index, type filter and a kind byte for the innermost `for ... in` loop | `IterLoad`, `IterNext` | `IterNext`, `KeyValueIter` |
| Saved iterators | A linked stack of outer loops' iterators | `IterPush` | `IterPop` |
| Catch frames | A linked stack, one node per open `try` | `Try` | `Catch`, `TryJmp`, a thrown error |
| Loop budget | A countdown that catches runaway loops | every `*Loop` jump and `TryJmp` | same |
| File and line | Where a runtime error says it happened | `DbgFile`, `DbgLine` | error reporting |

Two facts about this state that are easy to get wrong:

- **There is no separate cache stack.** `PushCache` and `PushCacheKey` save the
  register into the operand stack itself, at index 0 (the bottom), and shift
  every live operand up one slot.
- **The test flag and "a Number on the stack" are different results.** Some
  predicates write only the flag and push nothing. The compiler follows those
  with `GetFlag` when it wants the answer as a value. Treating one kind as the
  other leaves the stack one entry off from that point on.

## Text syntax

`dmasm` prints assembly it cannot fully read back. What is real today:

- One instruction, label or comment per line.
- A comment starts with `;`.
- A label is an identifier followed by `:`. Disassembled labels print as
  `LAB_XXXX`, the target word offset in four uppercase hex digits.
- An instruction is its name followed by its operands, separated by spaces.

The text parser only reads `u32` (decimal), `i32` and `Label` operands. Every
other operand type prints but does not parse, and there is no public function
that parses a whole document.

## Operands

| Operand | Words | Printed as | Encoding |
|---|---|---|---|
| `u32` | 1 | decimal | the word itself |
| `i32` | 1 | signed decimal | the word's bits, read as signed |
| `Label` | 1 | `LAB_0012` or a name | absolute word offset in the proc |
| `DMString` | 1 | `"text"` | string-table id. An id that falls in `0xFFCD..=0xFFEF` is stored with bit `0x1000_0000` set, so it cannot be mistaken for an access modifier |
| `Proc` | 1 | the proc's path | proc-table id |
| `Value` | 2, or 3 for a Number | see "Value" | see "Value" |
| `Variable` | 1 or more | see "Variable" | see "Variable" |
| `TypeFilter` | 1 | `(mob \| obj \| )` | bit flags, see "Type filter" |
| `IsInParams` | 1 | `Range`, `Value`, `BlockCorners`, `BlockCoords` | `0x0B`, `0x05`, `0x06`, `0x13` |
| `SwitchParams` | varies | `default => L, v => L, ` | count, then `(Value, Label)` pairs, then the default `Label` |
| `PickSwitchParams` | varies | same | count, then `(u32 threshold, Label)` pairs, then the default `Label`. A threshold is the running total of the weights so far, scaled so that all the weights together make 65535 |
| `SwitchRangeParams` | varies | same, ranges as `(lo to hi) => L` | range count, `(Value lo, Value hi, Label)` triples, then a `SwitchParams` |
| `PickProbParams` | varies | `L, L, ` | count, then that many `Label`s |

### Value

A constant DM value. Most take two words:

```text
word0 = tag in bits 0..7, data bits 16..23 in bits 8..15
word1 = data bits 0..15
```

So a constant's data is at most 24 bits. A Number takes three words instead:
`0x2A`, the upper 16 bits of the `f32`, the lower 16 bits.

Tags `dmasm` accepts in a `Value` operand, and how it prints them:

| Tag | Printed as |
|---|---|
| `0x00` with data 0 | `null` |
| `0x06` | a quoted string |
| `0x2A` | a number |
| `0x08` `0x09` `0x0A` `0x0B` `0x24` `0x26` `0x28` `0x3B` `0x3F` `0x59` | the type or proc path |
| `0x20` | `ref(...)`, kept as raw tag and data so a consumer can rebuild the datum type path |
| `0x0C` | `'resource path'` |
| `0x27` with data 0 | `/file` |
| `0x29` | `ref(...)` |

Any other tag stops disassembly with an unknown-value error.

### Variable

Names something to read, write or call. The first word decides the form:

- A word outside `0xFFCD..=0xFFEF` is a string id. That is a field read on
  whatever is in `cache`, printed `cache["name"]`.
- A word inside that range is an access modifier, followed by its own operands.

| Word | Form | Extra operands | Refers to |
|---|---|---|---|
| `0xFFCD` | `usr` | - | the proc's `usr` |
| `0xFFCE` | `src` | - | the proc's `src` |
| `0xFFCF` | `args` | - | the argument list |
| `0xFFD0` | `dot` | - | the `.` variable |
| `0xFFD5` | `*var` | a `Variable` | pointer dereference |
| `0xFFD8` | `cache` | - | the `cache` register |
| `0xFFD9` | `arg(n)` | `u32` | argument `n` |
| `0xFFDA` | `local(n)` | `u32` | local `n` |
| `0xFFDB` | `global("name")` | `u32` | slot in the global variable array. `dmasm` prints the name, but the slot is what identifies it: two `var/static` with one name in one proc are two slots |
| `0xFFDC` | `cache = a; b` | two `Variable`s | load `a` into `cache`, then evaluate `b` against it. This is how `a.b` is spelled |
| `0xFFDD` | `dynamic_proc("name")` | `DMString` | a proc looked up by name on `cache` |
| `0xFFDE` | `dynamic_verb("name")` | `DMString` | same, for a verb |
| `0xFFDF` | `static_proc(path)` | `Proc` | a proc by id |
| `0xFFE0` | `static_verb(path)` | `Proc` | a verb by id |
| `0xFFE3` | `cache_key` | - | the `cache_key` register |
| `0xFFE4` | `cache[cache_key]` | - | the element of `cache` at `cache_key` |
| `0xFFE5` | `world` | - | the world |
| `0xFFE6` | `null` | - | null |
| `0xFFE7` | `initial(var)` | a `Variable` | the variable's compile-time default |
| `0xFFE8` | `issaved(var)` | a `Variable` | whether the variable is saved |
| `0xFFEF` | `&var` | a `Variable` | pointer to the variable |

`0xFFD1`-`0xFFD4`, `0xFFD6`, `0xFFD7`, `0xFFE1`, `0xFFE2` and `0xFFE9`-`0xFFEE`
are in the range but `dmasm` has no form for them. Meeting one stops
disassembly.

The interpreter does not decode a `Variable` inside each instruction's handler.
Two shared functions do it, one for reads and one for writes, and they read the
operand words themselves. That is why `SetVar`'s handler contains no operand
decoding at all.

A call through a bare `dynamic_proc` (no `cache = ...;` in front) means the
receiver is already in `cache` from an earlier instruction, possibly across a
branch. `o?.f(a)` is the usual source: `SetCacheJmpIfNull` loads `cache`, and
the `Call` after it has no `cache = ...;` of its own.

### Argument counts, and the 65535 marker

An `arg_count` operand says how many Values the instruction takes off the
stack. On `Call`, `CallStatement` and `CallParentArgs` the value **65535 is not
a count**. It means one Value is on the stack and it is a list to spread over
the callee's parameters, which is what `f(arglist(L))` and `f(a = 1, b = 2)`
both compile to. Global calls never carry the marker; they use
`CallGlobalArgList` instead. § "Calls" lists which call shape gets which
encoding.

### Type filter

The second operand of `IterLoad`. It carries the coarse category of a typed
loop variable (`for(var/mob/M in L)`) or an `as` clause.

| Bit | Name | Bit | Name |
|---|---|---|---|
| `0x1` | mob | `0x200` | icon |
| `0x2` | obj | `0x400` | sound |
| `0x4` | text | `0x800` | message |
| `0x8` | num | `0x1000` | anything |
| `0x10` | file | `0x4000` | datum instances |
| `0x20` | turf | `0x8000` | password |
| `0x40` | key | `0x10000` | command text |
| `0x80` | null | `0x20000` | color |
| `0x100` | area | | |

A bit `dmasm` does not know stops disassembly.

## Instructions

How to read the table:

- **Instruction** is the declaration from `src/instructions.rs`, operands
  included.
- **Stack** is `taken -> left`, counted in Values. `flag` means the instruction
  writes the test flag. `?` means nobody has established it.
- Stack contents are written bottom to top, so in `list, index` the index is on
  top.
- **Basis** says how the row is known:

| Basis | Meaning |
|---|---|
| `read` | The interpreter's handler was read in the BYOND binary, or the behavior was measured on a live world, or both |
| `jit` | A JIT compiler built on `dmasm` compiles this instruction. It declares the stack effect and checks it on every compile, and its test suite compares results against the interpreter |
| `asm` | The interpreter's handler was read once in disassembly on 2026-10-04 (516.1687 Windows) for this file, sometimes with the functions it calls. Where a row says what DM source compiles to, that came from compiling a throwaway world with `dm.exe` 516.1687 and disassembling it with `dmasm`. Nothing was run |
| `dmasm` | `dmasm`'s own expression compiler emits it for that builtin with that many arguments (`src/compiler/builtin_procs.rs`). The single pushed result is assumed from the compiler treating it as a value. Nothing here has read the handler |
| `dm` | `dm.exe` 516.1687 compiled a small proc that uses the builtin, and `dmasm` disassembled it (2026-10-04). **Taken** is how many values the compiler pushed just before the instruction. **Left** comes from what the compiler did next: `Ret` or `Pop` means one value, `GetFlag` means the result went to the test flag, nothing means zero. The handler was not read, so what the instruction does with its values is the builtin's documented behavior, not something checked here |
| `name` | Only the name and the operand shape. Treat the behavior as a guess |

| Op | Instruction | Stack | Behavior | Basis |
|---|---|---|---|---|
| `0x00` | `End` | 0 -> 0 | Returns `dot` from the proc. Every proc ends in one | jit |
| `0x01` | `New(arg_count: u32)` | n+1 -> 1 | `new T(args)`. Stack is `type, arg0..argN-1`. Allocates and runs `New()` before the next instruction | jit |
| `0x02` | `Format(pattern: DMString, arg_count: u32)` | n -> 1 | String interpolation, `"[x] and [y]"`. The pattern marks each embed with a `0xFF` byte plus a code. An embedded datum runs its `operator""()` | jit |
| `0x03` | `Output` | 2 -> 0 | `target << value`. Stack is `target, value` | dm |
| `0x04` | `OutputFormat(pattern: DMString, arg_count: u32)` | n+1 -> 0 | `target << "text [x]"`: `Format` and `Output` in one instruction. Stack is `target`, then one value per embed | dm |
| `0x05` | `Stat` | 2 -> 0 | `stat(name, value)` | dm |
| `0x07` | `Link` | 2 -> 0 | `target << link(url)`. Stack is `target, url` | dm |
| `0x08` | `OutputFtp` | 3 -> 0 | `target << ftp(file, name)`. Stack is `target, file, name`. The compiler pushes null when the name is left out | dm |
| `0x09` | `OutputRun` | 2 -> 0 | `target << run(file)`. Stack is `target, file` | dm |
| `0x0A` | `OutputKey` | 2 -> 0 | `target << key(file)`. Stack is `target, file`. The current compiler accepts the statement with no warning. Sends the file through the helper `OutputFtp` uses, with no file name and mode 2 (`ftp` is 0, `browse` of a file is 3, `browse_rsc` is 4; `run` was 1 in the 2013 build). Raises `bad file` for a value that is not file-like. What the receiving client does with mode 2 was read only in the 2013 build 500.1205: it saves the file to a temporary file without asking the player, then does nothing with it | asm |
| `0x0B` | `Missile` | 3 -> 0 | `missile(type, start, end)` | dm |
| `0x0C` | `Del` | 1 -> 0 | `del x`. For `del(v)` on a var the compiler pushes the value, stores null into `v`, then emits `Del` | dm |
| `0x0D` | `Test` | 1 -> 0, flag | Sets the flag to whether the value is true. Null, `0`, `""` and an erased datum are false; NaN, `"0"` and an empty list are true | read |
| `0x0E` | `Not` | 1 -> 1 | Replaces the value with the Number 1 or 0, the opposite of its truth | read |
| `0x0F` | `Jmp(destination: Label)` | 0 -> 0 | Unconditional jump | read |
| `0x10` | `Jnz(destination: Label)` | 0 -> 0 | Jumps when the test flag is set | asm |
| `0x11` | `Jz(destination: Label)` | 0 -> 0 | Jumps when the test flag is clear | asm |
| `0x12` | `Ret` | 1 -> 0 | Returns the top of the stack from the proc | jit |
| `0x13` | `IsLoc` | 1 -> 0, flag | `isloc(x)`. **No tag test at all**, only "does this atom still exist" | read |
| `0x14` | `IsMob` | 1 -> 0, flag | `ismob(x)`. Tag `0x03` and the atom must still exist | read |
| `0x15` | `IsObj` | 1 -> 0, flag | `isobj(x)`. Tag `0x02` and the atom must still exist | read |
| `0x16` | `IsArea` | 1 -> 0, flag | `isarea(x)`. Tag `0x04` and the atom must still exist | read |
| `0x17` | `IsTurf` | 1 -> 0, flag | `isturf(x)`. Tag `0x01` and the atom must still exist | read |
| `0x18` | `Alert` | 6 -> 1 | `alert()`. `dmasm`'s compiler always pushes six arguments, null for the missing ones. Parks the proc once the prompt is sent | dmasm |
| `0x19` | `EmptyList` | 1 -> 1 | Despite the name it takes a length and pushes a new list of that many nulls. Emitted for `var/list/L[n]` | jit |
| `0x1A` | `NewList(arg_count: u32)` | n -> 1 | `list(a, b, c)` | jit |
| `0x1B` | `View` | 2 -> 1 | `view(dist, center)` used as an expression. Either argument may be null and the two may come in either order | dmasm |
| `0x1C` | `OView` | 2 -> 1 | `oview(dist, center)`, same argument rules | dmasm |
| `0x1D` | `ViewTarget` | 2 -> 1 | `view(dist, center)` written as the left side of `<<`, as in `view() << "hi"`. Pushes the shared scratch list itself, with no copy (§ "The shared scratch list") | asm |
| `0x1E` | `OViewTarget` | 2 -> 1 | The same for `oview()` | asm |
| `0x1F` | `Block` | 2 -> 1 | `block(start, end)` used as an expression | dmasm |
| `0x20` | `BlockTarget` | 2 -> 1 | `block(start, end)` written as the left side of `<<`. Pushes the shared scratch list itself, with no copy | asm |
| `0x21` | `Prob` | 1 -> 1 | `prob(p)`. Pushes the Number 1 or 0 | jit |
| `0x22` | `Rand` | 1 -> 1 | `rand()` and `rand(high)`. The compiler pushes null for the no-argument form. BYOND's own `/generator/proc/Rand` uses it too, on the generator's handle | dm |
| `0x23` | `RandRange` | 2 -> 1 | `rand(low, high)` | dm |
| `0x24` | `Sleep` | 1 -> 0 | `sleep(n)`. `n >= 0` parks the proc and its callers. `-5 < n < 0` parks only when the server is behind. `n <= -5` runs a string-table self-check and does not park | read |
| `0x25` | `Spawn(destination: Label)` | 1 -> 0 | `spawn(delay) { body }`. The label is where the **parent** continues, not where the child starts: the frame is copied with its position already on the body, then the parent jumps to the label. The parent never yields | read |
| `0x27` | `BrowseRsc` | 3 -> 0 | `target << browse_rsc(file, name)`. Stack is `target, file, name`. The compiler pushes null when the name is left out | dm |
| `0x28` | `IsIcon` | 1 -> 1 | `isicon(x)`. Not a tag test: it reads the resource to check the icon header | read |
| `0x29` | `Call(proc: Variable, arg_count: u32)` | n -> 1 | Calls the proc the `Variable` names. `a.f(x)` is `cache = a; dynamic_proc("f")`. A bare `dynamic_proc` uses whatever is in `cache`. See "Argument counts" for 65535 | jit |
| `0x2A` | `CallStatement(proc: Variable, arg_count: u32)` | n -> 1 | Same shape, and it still pushes a result: the compiler puts a `Pop` after it. What separates it from `Call` is not established. It is **not** "value position versus statement position" | jit |
| `0x2B` | `CallPath(arg_count: u32)` | n+1 -> 1 | `call(p)(args)`. Stack is `proc, arg0..argN-1`. A proc path calls like `CallGlob`. A string goes through `text2path()` first, so only a full path such as `"/proc/foo"` works | read |
| `0x2C` | `CallParent` | 0 -> 1 | `..()` with no arguments written: forwards the proc's own arguments. With no DM parent it calls BYOND's builtin of the same name | jit |
| `0x2D` | `CallParentArgs(arg_count: u32)` | n -> 1 | `..(a, b)` | jit |
| `0x2E` | `CallSelf` | 0 -> 1 | `.()`, the proc calling itself with nothing pushed | dm |
| `0x2F` | `CallSelfArgs(arg_count: u32)` | n -> 1 | `.(a, b)`, the proc calling itself with `arg_count` arguments | dm |
| `0x30` | `CallGlob(arg_count: u32, proc: Proc)` | n -> 1 | Calls a global proc by id. The count word comes **before** the proc id | jit |
| `0x31` | `Log10` | 1 -> 1 | **Misnamed.** One-argument `log(x)`, the natural log. Raises for x <= 0 | read |
| `0x32` | `Log` | 2 -> 1 | `log(base, x)`. Stack is `base, x`. Not base 10. Raises for x <= 0, base <= 0 or base == 1 | read |
| `0x33` | `GetVar(var: Variable)` | 0 -> 1 | Pushes the variable's value | jit |
| `0x34` | `SetVar(var: Variable)` | 1 -> 0 | Stores the top of the stack into the variable | read |
| `0x35` | `SetVarExpr(var: Variable)` | 1 -> 1 | `SetVar` that leaves the value on the stack, for an assignment used as a value: `b = (a = 7)` | read |
| `0x36` | `GetFlag` | 0 -> 1 | Pushes the test flag as the Number 1 or 0 | asm |
| `0x37` | `Teq` | 2 -> 1 | `a == b`. Pushes the Number 1 or 0 | jit |
| `0x38` | `Tne` | 2 -> 1 | `a != b` | jit |
| `0x39` | `Tl` | 2 -> 1 | `a < b` | jit |
| `0x3A` | `Tg` | 2 -> 1 | `a > b` | jit |
| `0x3B` | `Tle` | 2 -> 1 | `a <= b` | jit |
| `0x3C` | `Tge` | 2 -> 1 | `a >= b` | jit |
| `0x3D` | `UnaryNeg` | 1 -> 1 | `-x` | jit |
| `0x3E` | `Add` | 2 -> 1 | `a + b`. Branches on the left operand's tag: numbers add, strings join, a list on the left builds a **new** list with the right side folded in one level deep. A null operand yields the other operand unchanged. See "Operators" below | read |
| `0x3F` | `Sub` | 2 -> 1 | `a - b`. Numbers subtract. A list on the left builds a new list without the right side's elements. Null counts as 0 | read |
| `0x40` | `Mul` | 2 -> 1 | `a * b`. Null counts as 0 | jit |
| `0x41` | `Div` | 2 -> 1 | `a / b`. A zero divisor raises `Division by zero`. A null divisor raises `Undefined operation` instead | read |
| `0x42` | `Mod` | 2 -> 1 | `a % b`. Truncates both sides to integers first. A zero or null divisor raises `Division by zero` | read |
| `0x43` | `Round` | 1 -> 1 | One-argument `round(x)`, which is `floor(x)` | jit |
| `0x44` | `RoundN` | 2 -> 1 | `round(a, b)`: `floor(a / b + 0.5) * b` | jit |
| `0x45` | `AugAdd(var: Variable)` | 1 -> 0 | `var += x`. On a list it changes the list in place, so its identity is kept | read |
| `0x46` | `AugSub(var: Variable)` | 1 -> 0 | `var -= x`, in place on a list | read |
| `0x47` | `AugMul(var: Variable)` | 1 -> 0 | `var *= x` | jit |
| `0x48` | `AugDiv(var: Variable)` | 1 -> 0 | `var /= x` | jit |
| `0x49` | `AugMod(var: Variable)` | 1 -> 0 | `var %= x` | jit |
| `0x4A` | `AugBand(var: Variable)` | 1 -> 0 | `var &= x`, in place on a list | read |
| `0x4B` | `AugBor(var: Variable)` | 1 -> 0 | `var \|= x`, in place on a list | read |
| `0x4C` | `AugXor(var: Variable)` | 1 -> 0 | `var ^= x`, in place on a list | read |
| `0x4D` | `AugLShift(var: Variable)` | 1 -> 0 | `var <<= x` | jit |
| `0x4E` | `AugRShift(var: Variable)` | 1 -> 0 | `var >>= x` | jit |
| `0x50` | `PushInt(value: i32)` | 0 -> 1 | Pushes the integer as a Number | jit |
| `0x51` | `Pop` | 1 -> 0 | Discards the top of the stack | jit |
| `0x52` | `IterLoad(unk0: u32, types: TypeFilter)` | 1, 2 or 6 -> 0 | Starts a `for ... in` loop. The first operand is the iterator kind: 5 for a list or anything list-like (takes 1), 20 for `for(k, v in L)` (takes 1), 6 for `block(a, b)` (takes 2), 7 `view`, 8 `oview`, 13 `range`, 14 `orange` and 15-18 for the rest of that family (take 2), 19 for six-argument `block()` (takes 6). A list is copied at this point, so changing it mid-loop does not change the walk. Null, a number or a string loop zero times | read |
| `0x53` | `IterNext` | 0 -> 1, flag | Sets the flag to whether an element was left and pushes it, or null when the loop is done. Skips elements the type filter rejects. Kind 20 pushes two Values, the key then the value | read |
| `0x54` | `IterPush` | 0 -> 0 | Saves the active iterator before a nested loop starts | read |
| `0x55` | `IterPop` | 0 -> 0 | Frees the active iterator and restores the saved one. Only emitted to close a nested loop; a top-level loop gets neither this nor `IterPush` | read |
| `0x56` | `Num2TextSigFigs` | 2 -> 1 | `num2text(x, digits)`. Stack is `x, digits`. `sprintf("%.*g", clamp(digits, 0, 100), x)` | read |
| `0x57` | `Roll` | 2 -> 1 | `roll(dice, sides)` | dm |
| `0x58` | `NewListTarget(arg_count: u32)` | n -> 1 | `list(a, b)` written as the left side of `<<`. Moves `arg_count` values into the shared scratch list and pushes it, with no copy | asm |
| `0x59` | `Range` | 2 -> 1 | `range(dist, center)`. Fills the shared scratch list and pushes it, with no copy (§ "The shared scratch list"). The compiler puts `CopyList` right after it, except when the range is the left side of `<<` | asm |
| `0x5A` | `LocatePos` | 3 -> 1 | `locate(x, y, z)`. Truncates toward zero. 0, a negative, an out-of-range or a non-number coordinate gives null, never an error | read |
| `0x5B` | `LocateRef` | 1 -> 1 | **The name is narrower than the opcode.** This is all of one-argument `locate(X)`: a `[0x...]` reference string, an atom `tag` string, or an atom type path (first instance of that type). `locate(/datum/foo)` is always null. `locate(/area/foo)` creates the area when none exists | read |
| `0x5C` | `Flick` | 2 -> 0 | `flick(icon, object)` | dm |
| `0x5D` | `Shutdown` | 0 -> 0 | `shutdown()` with no arguments | dm |
| `0x5E` | `Startup(arg_count: u32)` | n -> ? | `startup()`. Parks the proc once the child world starts | dmasm |
| `0x5F` | `RollStr` | 1 -> 1 | One-argument `roll(x)`, such as `roll("2d6")` | dm |
| `0x60` | `PushVal(value: Value)` | 0 -> 1 | Pushes a constant | read |
| `0x61` | `NewImage` | 2 -> 1 | Two-argument `image(icon, loc)`. Six arguments compile to `NewImageArgs`, named arguments to `NewImageArgList`. Other counts were not probed | dm |
| `0x62` | `PreInc(var: Variable)` | 0 -> 1 | `++var` as a value: adds 1, pushes the new value | jit |
| `0x63` | `PostInc(var: Variable)` | 0 -> 1 | `var++` as a value: pushes the old value, then adds 1. On a null variable the pushed old value is null, not 0 | jit |
| `0x64` | `PreDec(var: Variable)` | 0 -> 1 | `--var` as a value | jit |
| `0x65` | `PostDec(var: Variable)` | 0 -> 1 | `var--` as a value | jit |
| `0x66` | `Inc(var: Variable)` | 0 -> 0 | `var++` as a statement | jit |
| `0x67` | `Dec(var: Variable)` | 0 -> 0 | `var--` as a statement | jit |
| `0x68` | `Abs` | 1 -> 1 | `abs(x)` | jit |
| `0x69` | `Sqrt` | 1 -> 1 | `sqrt(x)` | jit |
| `0x6A` | `Pow` | 2 -> 1 | `base ** exponent`. Stack is `base, exponent` | jit |
| `0x6B` | `Turn` | 2 -> 1 | `turn(A, angle)`. A number is a direction and rotates in 45 degree steps, throwing the remainder away. Also handles icons, matrices and vectors. An atom's own `operator_turn` never fires | read |
| `0x6C` | `AddText(arg_count: u32)` | n -> 1 | `addtext(a, b, ...)` | jit |
| `0x6D` | `Length` | 1 -> 1 | `length(x)`. For a string this is its length in bytes | jit |
| `0x6E` | `CopyText` | 3 -> 1 | `copytext(text, start, end)` | dmasm |
| `0x6F` | `FindText` | 4 -> 1 | `findtext(haystack, needle, start, end)`. Positions count bytes. Case folding covers all of Unicode, not just ASCII | jit |
| `0x70` | `FindTextEx` | 4 -> 1 | `findtextEx(...)`, the case-sensitive form | dmasm |
| `0x71` | `CmpText` | 2 -> 1, flag | `cmptext(a, b, ...)`. Compares the top two values into the test flag and leaves the lower one, the same shape as `Teq`. The compiler chains it: `a, b, CmpText, Jz end, c, CmpText`, then `Pop` and `GetFlag` | dm |
| `0x72` | `SortText(arg_count: u32)` | n -> 1 | `sorttext(a, b, ...)` | dmasm |
| `0x73` | `SortTextEx(arg_count: u32)` | n -> 1 | `sorttextEx(a, b, ...)` | dmasm |
| `0x74` | `UpperText` | 1 -> 1 | `uppertext(t)`. Unicode-aware | jit |
| `0x75` | `LowerText` | 1 -> 1 | `lowertext(t)`. Unicode-aware | jit |
| `0x76` | `Text2Num` | 1 -> 1 | `text2num(t)`. Null when the text is not a number | jit |
| `0x77` | `Num2Text` | 1 -> 1 | `num2text(x)`: `sprintf("%.*g", 6, x)`, so six significant digits | read |
| `0x78` | `Switch(params: SwitchParams)` | 1 -> 0 | `switch(x)` with plain cases. Jumps to the first case whose tag and data both match exactly, else to the default | read |
| `0x79` | `PickSwitch(params: PickSwitchParams)` | 0 -> 0 | `pick()` with the options written out and weights the compiler knows: `pick(a, b, c)`, `pick(50; a, 200; b)`, and the statement form. Draws one 16-bit random number and jumps to the first label whose threshold is not below it, or to the default label when none is. It never touches the stack: the code at each label pushes that option | asm |
| `0x7A` | `SwitchRange(params: SwitchRangeParams)` | 1 -> 0 | `switch(x)` with `lo to hi` cases. Ranges are tried first and in order, and only a Number can match one. Then the plain cases | read |
| `0x7B` | `ListGet` | 2 -> 1 | `L[i]`. Stack is `list, index` | jit |
| `0x7C` | `ListSet` | 3 -> 0 | `L[i] = v`. Stack is `value, list, index`. Writing past the end raises and leaks one reference to the value | jit |
| `0x7D` | `IsType` | 2 -> 1 | `istype(value, type)`. Stack is `value, type`. Pushes a Number and leaves the test flag alone | read |
| `0x7E` | `Band` | 2 -> 1 | `a & b`. Numbers are 24-bit integers. A list on the left intersects, keeping the left side's order and dropping duplicates | read |
| `0x7F` | `Bor` | 2 -> 1 | `a \| b`. A list on the left unions. It removes duplicates by subtraction, so `list(1) \| list(1,1)` is `list(1,1)`. A null operand yields the other unchanged | read |
| `0x80` | `Bxor` | 2 -> 1 | `a ^ b`. A list on the left gives the elements only one side has. A null operand yields the other unchanged | read |
| `0x81` | `Bnot` | 1 -> 1 | `~x`, 24 bits | jit |
| `0x82` | `LShift` | 2 -> 1 | `a << b`, 24 bits | jit |
| `0x83` | `RShift` | 2 -> 1 | `a >> b`, 24 bits | jit |
| `0x84` | `DbgFile(name: DMString)` | 0 -> 0 | Records the source file for error messages | read |
| `0x85` | `DbgLine(line: u32)` | 0 -> 0 | Records the source line | read |
| `0x86` | `Step` | 2 -> 0, flag | `step(ref, dir)`. Whether it moved goes to the test flag | dm |
| `0x87` | `StepTo` | 3 -> 0, flag | `step_to(ref, target, min)` | dm |
| `0x88` | `StepAway` | 3 -> 0, flag | `step_away(ref, target, max)` | dm |
| `0x89` | `StepTowards` | 2 -> 0, flag | `step_towards(ref, target)` | dm |
| `0x8A` | `StepRand` | 1 -> 0, flag | `step_rand(ref)` | dm |
| `0x8B` | `Walk` | 3 -> 0 | `walk(ref, dir, lag)` | dm |
| `0x8C` | `WalkTo` | 4 -> 0 | `walk_to(ref, target, min, lag)` | dm |
| `0x8D` | `WalkAway` | 4 -> 0 | `walk_away(ref, target, max, lag)` | dm |
| `0x8E` | `WalkTowards` | 3 -> 0 | `walk_towards(ref, target, lag)` | dm |
| `0x8F` | `WalkRand` | 2 -> 0 | `walk_rand(ref, lag)` | dm |
| `0x90` | `GetStep` | 2 -> 1 | `get_step(ref, dir)`. Pushes the neighboring turf or null. Follows `ref`'s `loc` chain to a turf first. Direction bits `0x10` and `0x20` move up and down a z level | read |
| `0x91` | `GetStepTo` | 3 -> 1 | `get_step_to(ref, target, min)` | dmasm |
| `0x92` | `GetStepAway` | 3 -> 1 | `get_step_away(ref, target, max)` | dmasm |
| `0x93` | `GetStepTowards` | 2 -> 1 | `get_step_towards(ref, target)` | dmasm |
| `0x94` | `GetStepRand` | 1 -> 1 | `get_step_rand(ref)` | dmasm |
| `0x95` | `GetDist` | 2 -> 1 | `get_dist(a, b)`: `max(abs(dx), abs(dy))`. **Ignores z.** `-1` when both are the same object, infinity when either is not on a turf | read |
| `0x96` | `GetDir` | 2 -> 1 | `get_dir(a, b)`. A direction bit field, or 0 when either is not on a turf. Ignores z | read |
| `0x97` | `LocateType` | 2 -> 1 | `locate(T) in C`. Stack is `T, C`. First element of `C` whose type is `T` or a subtype. A string `T` is an atom `tag` lookup, never a type path. `C == world` scans every instance | read |
| `0x98` | `Shell` | 1 -> 1 | `shell(command)`. Parks the proc once the command starts | dm |
| `0x99` | `Text2File` | 2 -> 1 | `text2file(text, file)` | dmasm |
| `0x9A` | `File2Text` | 1 -> 1 | `file2text(path)` | dmasm |
| `0x9B` | `FCopy` | 2 -> 1 | `fcopy(src, dst)` | dmasm |
| `0x9E` | `IsNull` | 1 -> 1 | `isnull(x)`. True for tag `0x00` only | read |
| `0x9F` | `IsNum` | 1 -> 1 | `isnum(x)`. True for tag `0x2A` only | read |
| `0xA0` | `IsText` | 1 -> 1 | `istext(x)`. True for tag `0x06` only | read |
| `0xA1` | `StatPanel` | 3 -> 0 | `statpanel(panel, name, value)` | dm |
| `0xA2` | `StatPanelCheck` | 1 -> 0, flag | The one-argument `statpanel(panel)` check. The answer goes to the test flag | dm |
| `0xA3` | `WaitforBegin` | 0 -> 0 | Start of the old `waitfor` block statement. The compiler still accepts it and warns `waitfor is now obsolete, since it is the default behavior`. Adds 1 to a one-byte counter in the frame. No named proc in nine compiled SS13 codebases contained one when they were swept on 2026-09-16 | asm |
| `0xA4` | `WaitforEnd` | 0 -> 0 | End of that block. Subtracts 1 from the same counter | asm |
| `0xA5` | `Min(arg_count: u32)` | n -> 1 | `min(a, b, ...)` | jit |
| `0xA6` | `Max(arg_count: u32)` | n -> 1 | `max(a, b, ...)` | jit |
| `0xA7` | `TypesOf(arg_count: u32)` | n -> 1 | `typesof(a, ...)` | dmasm |
| `0xA8` | `CKey` | 1 -> 1 | `ckey(t)`. Drops every byte outside `a-z0-9` after lowercasing | jit |
| `0xA9` | `IsIn(params: IsInParams)` | 2, 3 or 7 -> 0, flag | `x in ...`. `Value`: stack `container, x`. `Range` (`x in lo to hi`): stack `lo, hi, x`, true when `hi >= x && x >= lo`, and a non-number reads as 0. `BlockCorners`: stack `a, b, x`. `BlockCoords`: stack of six coordinates then `x`, padded with nulls by the compiler | read |
| `0xAA` | `Browse` | 2 -> 0 | `target << browse(body)`. Stack is `target, body` | dm |
| `0xAB` | `BrowseOpt` | 3 -> 0 | `target << browse(body, options)`. Stack is `target, body, options` | dm |
| `0xAC` | `FList` | 1 -> 1 | `flist(path)` | dmasm |
| `0xAD` | `ORange` | 2 -> 1 | `orange(dist, center)`. Same as `Range`: pushes the shared scratch list, and `CopyList` follows unless it is the left side of `<<` | asm |
| `0xAE` | `CopyList` | 1 -> 1 | Replaces the list on top with a private copy. The compiler emits it after `Range` and `ORange` | asm |
| `0xAF` | `Read` | 1 -> 1 | `source >> var`. Takes the source and pushes what it read; a `SetVar` after it stores the value | dm |
| `0xB0` | `Index` | 2 -> 1 | An older way to index. Stack is `container, index`. A string index needs a savefile underneath and gives that savefile path; anything else under a string raises `bad savefile or list`. Any other index becomes a whole number and is looked up by 1-based position in the container, in a helper that switches on the container's tag and was not read further. `dm.exe` 516.1687 compiles both `F["x"]` and `L[i]` to `ListGet` instead | asm |
| `0xB1` | `PickProb(params: PickProbParams)` | n -> 0 | `pick(prob(w); a, b)`. Takes one weight per label and jumps to the chosen label, where the compiler pushes that option. An option with no `prob()` gets a weight of 100 | dm |
| `0xB2` | `JmpOr(destination: Label)` | 1 -> 0, or stays on the jump | `a \|\| b`. A true value stays on the stack and the jump is taken. A false one is popped and execution falls through | read |
| `0xB3` | `JmpAnd(destination: Label)` | 1 -> 0, or stays on the jump | `a && b`. A false value stays and the jump is taken. A true one is popped | read |
| `0xB4` | `FDel` | 1 -> 1 | `fdel(path)` | dmasm |
| `0xB5` | `CallName(arg_count: u32)` | n+2 -> 1 | `call(recv, "name")(args)`. Stack is `recv, name, arg0..argN-1`. A string receiver is the old library-call spelling and takes the `call_ext` path | read |
| `0xB6` | `ShutdownArgs` | 2 -> 1 | `shutdown(addr, natural)`. Parks the proc when `addr` names a world this one started | dm |
| `0xB7` | `List2Params` | 1 -> 1 | `list2params(L)` | dmasm |
| `0xB8` | `Params2List` | 1 -> 1 | `params2list(text)` | dmasm |
| `0xB9` | `CKeyEx` | 1 -> 1 | `ckeyEx(t)` | dmasm |
| `0xBA` | `PromptCheck` | 0 -> 0 | The compiler puts one after every `Input` and `InputColor`. Looks at the value on top without taking it: tag `0x22` raises `empty prompt list`, anything else passes | asm |
| `0xBB` | `Rgb` | 3 -> 1 | `rgb(r, g, b)` | dm |
| `0xBC` | `HasCall` | 2 -> 1 | `hascall(obj, name)` | dmasm |
| `0xBE` | `HtmlEncode` | 1 -> 1 | `html_encode(t)` | dmasm |
| `0xBF` | `HtmlDecode` | 1 -> 1 | `html_decode(t)` | dmasm |
| `0xC0` | `Time2Text` | 2 -> 1 | `time2text(t)` and `time2text(t, format)`. The compiler pushes a null format for the one-argument call. Still the common form in 516 | read |
| `0xC1` | `Input(unk0: u32, unk1: u32, unk2: u32)` | 4 or 5 -> 1 | `input()`. Stack is `recipient, message, title, default`, with the `in` list under those four when `unk2` is 64. `unk0` is the `as` type as type-filter bits, 0 for none. `unk2` says where the choices come from: 0 nowhere, 64 a list on the stack, 16 `in world`. `unk1` was 0 in every probe except 127 for `in world` and 3 for `in view(3, M)`, so it looks like a distance; not confirmed. When the recipient is left out the compiler pushes the other three one slot early and pads with nulls, and the handler uses `usr`. Parks the proc once the prompt is sent | asm |
| `0xC2` | `Sin` | 1 -> 1 | `sin(x)`, in degrees | jit |
| `0xC3` | `Cos` | 1 -> 1 | `cos(x)`, in degrees | jit |
| `0xC4` | `ArcSin` | 1 -> 1 | `arcsin(x)`, result in degrees | jit |
| `0xC5` | `ArcCos` | 1 -> 1 | `arccos(x)`, result in degrees | jit |
| `0xC6` | `InputColor(unk0: u32, unk1: u32, unk2: u32)` | 5 or 6 -> 1 | `input() as color`. Same code as `Input`, with one more number at the very bottom of its stack block that carries the type bits in place of `unk0`. The compiler pushes 131072 there for `as color` | asm |
| `0xC7` | `Crash` | 1 -> 0 | `CRASH(msg)` | dm |
| `0xC8` | `NewAssocList(arg_count: u32)` | 2n -> 1 | `list(k1 = v1, k2 = v2)`. Stack is `k1, v1, k2, v2, ...`. Also builds the list for a named-argument call | jit |
| `0xC9` | `CallParentArgList` | 1 -> 1 | `..(arglist(L))` and `..(a = 1)`. The one Value is the list. Shape from `dm.exe` output; the handler is not read | read |
| `0xCA` | `CallSelfArgList` | 1 -> 1 | `.(arglist(L))`. Takes the list | dm |
| `0xCB` | `CallPathArgList` | 2 -> 1 | `call(p)(arglist(L))`. Stack is `proc, list` | read |
| `0xCC` | `CallNameArgList` | 3 -> 1 | `call(recv, "name")(arglist(L))`. Stack is `recv, name, list` | read |
| `0xCD` | `CallGlobalArgList(proc: Proc)` | 1 -> 1 | A global call whose arguments are a list: `f(arglist(L))` and `f(a = 1)` | jit |
| `0xCF` | `NewArgList` | 2 -> 1 | `new T(arglist(L))` and `new T(a = 1)`. Stack is `type, list` | dm |
| `0xD0` | `MinList` | 1 -> 1 | `min(L)` on one list | dm |
| `0xD1` | `MaxList` | 1 -> 1 | `max(L)` on one list | dm |
| `0xD2` | `Pick` | 1 -> 1 | `pick(L)` on one list. `pick(a, b, c)` with the options written out compiles to `PickSwitch` | dm |
| `0xD3` | `NewImageArgList` | 1 -> 1 | `image(arglist(L))`, and `image(icon = a, loc = b)` with a `NewAssocList` built first. Takes the list | dm |
| `0xD4` | `NewImageArgs(arg_count: u32)` | n -> 1 | `image(a, b, ...)` with positional arguments. Seen with six | dm |
| `0xD7` | `FCopyRsc` | 1 -> 1 | `fcopy_rsc(path)` | dmasm |
| `0xD9` | `ShellAllowed` | 0 -> 1 | `shell()` with no arguments, which asks whether shell access is allowed | dm |
| `0xDA` | `RandSeed` | 1 -> 0 | `rand_seed(seed)` | dm |
| `0xDB` | `Text2Ascii` | 2 -> 1 | `text2ascii(text, pos)`. The compiler pushes null for a missing position | dmasm |
| `0xDC` | `Ascii2Text` | 1 -> 1 | `ascii2text(n)` | dmasm |
| `0xDD` | `IconStates` | 1 -> 1 | `icon_states(icon)` | dm |
| `0xDE` | `IconNew(arg_count: u32)` | n -> 1 | Builds the icon value behind an `/icon` datum. BYOND's own `/icon/New` pushes its five arguments, emits `IconNew 5` and stores the result in the datum's `icon` var; `/icon/proc/operator:=` uses `IconNew 1` | dm |
| `0xDF` | `TurnOrFlipIcon(filter_mode: u32, var: Variable)` | 1 -> 0 | `icon.Turn(angle)` or `icon.Flip(dir)` on the icon in `var`, written back in place. BYOND's own `/icon` procs use `filter_mode` 5 for `Turn`, 9 for `Turn` when its second argument is true, and 6 for `Flip`. Not `Turn` (`0x6B`) | dm |
| `0xE0` | `IconBlendSimple(var: Variable)` | 2 -> 0 | `_dm_icon_blend(icon, other, function)` with no position. Stack is `other, function`. Same code as `IconBlend`, with the position fixed at 1, 1. BYOND's own `/icon/proc/Blend` always passes a position, so this only comes from DM code that calls `_dm_icon_blend` itself | asm |
| `0xE1` | `IconIntensity(var: Variable)` | 3 -> 0 | `icon.SetIntensity(r, g, b)` on the icon in `var`. `/icon/proc/SetIntensity` swaps a null `g` or `b` for -1 first | dm |
| `0xE2` | `IconSwapColor(var: Variable)` | 2 -> 0 | `icon.SwapColor(old, new)` on the icon in `var` | dm |
| `0xE3` | `ShiftIcon(var: Variable)` | 3 -> 0 | `icon.Shift(dir, offset, wrap)` on the icon in `var` | dm |
| `0xE4` | `IsFile` | 1 -> 1 | `isfile(x)`. True for tag `0x0C` or `0x27` | read |
| `0xE5` | `Viewers` | 2 -> 1 | `viewers(depth, center)` | dm |
| `0xE6` | `OViewers` | 2 -> 1 | `oviewers(depth, center)` | dmasm |
| `0xE7` | `Hearers` | 2 -> 1 | `hearers(depth, center)` | dmasm |
| `0xE8` | `OHearers` | 2 -> 1 | `ohearers(depth, center)` | dmasm |
| `0xE9` | `DbNewConnection` | 0 -> 1 | `_dm_db_new_con()`, the internal proc behind the database datums | dmasm |
| `0xEA` | `DbNewQuery` | 0 -> 1 | `_dm_db_new_query()` | dmasm |
| `0xEB` | `DbConnect` | 6 -> 1 | `_dm_db_connect(...)` | dmasm |
| `0xEC` | `DbExecute` | 5 -> 1 | `_dm_db_execute(...)` | dmasm |
| `0xED` | `DbNextRow` | 3 -> 1 | `_dm_db_next_row(...)` | dmasm |
| `0xEE` | `DbErrorMsg` | 1 -> 1 | `_dm_db_error_msg(...)` | dmasm |
| `0xEF` | `DbClose` | 1 -> 1 | `_dm_db_close(...)` | dmasm |
| `0xF0` | `DbIsConnected` | 1 -> 1 | `_dm_db_is_connected(...)` | dmasm |
| `0xF1` | `DbRowsAffected` | 1 -> 1 | `_dm_db_rows_affected(...)` | dmasm |
| `0xF2` | `DbRowCount` | 1 -> 1 | `_dm_db_row_count(...)` | dmasm |
| `0xF3` | `DbQuote` | 2 -> 1 | `_dm_db_quote(...)` | dmasm |
| `0xF4` | `DbColumns` | 2 -> 1 | `_dm_db_columns(...)` | dmasm |
| `0xF5` | `IsPath` | 1 -> 1 | One-argument `ispath(x)`. True for the ten type path tags. A proc path is not one of them | read |
| `0xF6` | `IsSubPath` | 2 -> 1 | `ispath(val, type)`. `val` must be a type path; an instance there is always 0. `type` may be an instance | read |
| `0xF7` | `FExists` | 1 -> 1 | `fexists(path)`. Null for anything but a string or a file value. Can raise `Safety violation` when the server runs in safe mode | read |
| `0xF8` | `JmpLoop(destination: Label)` | 0 -> 0 | `Jmp` for a loop's backward jump. Also counts down the loop budget. At zero a normal proc logs "Infinite loop suspected", becomes a background proc and parks; a background proc parks when its time is up | read |
| `0xF9` | `JnzLoop(destination: Label)` | 0 -> 0 | `Jnz` with the same loop budget | read |
| `0xFA` | `JzLoop(destination: Label)` | 0 -> 0 | `Jz` with the same loop budget | read |
| `0xFB` | `PopN(count: u32)` | n -> 0 | Discards `count` Values. Seen only at the end of a `for(x in a to b)` loop, where it removes the loop's bounds | asm |
| `0xFC` | `Check2Numbers` | 0 -> 0 | Start of `for(x in a to b)`. Raises unless the top two Values are both Numbers. Takes nothing: the counter and the bound stay on the stack for the whole loop | asm |
| `0xFD` | `ForRange(exit: Label, var: Variable)` | 0 -> 0 | One pass of `for(x in a to b)`. Stack is `counter, bound`. If the counter is past the bound, jumps to `exit`. Otherwise stores the counter into `var` and adds 1 to the copy on the stack | asm |
| `0xFE` | `Check3Numbers` | 0 -> 0 | Start of `for(x in a to b step c)`. Raises unless the top three Values are all Numbers | asm |
| `0xFF` | `ForRangeStep(exit: Label, var: Variable)` | 0 -> 0 | One pass of the stepped loop. Stack is `counter, bound, step`. A step above 0 runs while `bound >= counter`; any other step runs while `counter >= bound`. Stores the counter into `var`, then adds the step | asm |
| `0x100` | `DmsNewKernel` | 0 -> 1 | Creates a kernel object for DM Script, a small script language built into BYOND (its classes are `DMSKernelData`, `DMSParser`, `DMSProcData`), and pushes a handle to it: a tag `0x45` value of kind 5. `0x100`-`0x104` are the VM's whole interface to it. No probe found DM source that compiles to any of the five | asm |
| `0x101` | `DmsParse` | 2 -> 1 | Stack is `kernel, text`. Runs a DM Script parser over the text for that kernel. Pushes a string on success and null otherwise | asm |
| `0x102` | `DmsExportText` | 2 -> 1 | Stack is `kernel, x`. Pushes the kernel's exported text as a string, or null when the first value is not a kernel. The second value is taken and never read | asm |
| `0x103` | `Eval` | 1 -> 1 | Runs DM Script. A string is compiled and run, and a compile failure raises a runtime error carrying the compiler's message. A prepared script handle is run as it is. Pushes the script's result, or null for any other input | asm |
| `0x104` | `DmsPrepare` | 2 -> 1 | Stack is `kernel, text`. Compiles the text against the kernel and pushes the compiled script. On a compile failure it appears to push the error text instead; that branch was read in the decompiler only | asm |
| `0x105` | `IconDrawBox(var: Variable)` | 5 -> 0 | `icon.DrawBox(color, x1, y1, x2, y2)` on the icon in `var` | dm |
| `0x106` | `IconInsert(arg_count: u32, var: Variable)` | n -> 1 | `icon.Insert(new_icon, icon_state, dir, frame, moving, delay)`. BYOND's own `/icon/proc/Insert` pushes the icon and those six, count 7. Also writes the icon in `var`; the proc pops the pushed value. Until 2026-10-04 `dmasm` declared no `var` here, and printed that word and the `Pop` after it as a made-up `CallPath 81` | asm |
| `0x107` | `UrlEncode` | 2 -> 1 | `url_encode(text, format)`. The compiler pushes null for a missing format | dmasm |
| `0x108` | `UrlDecode` | 1 -> 1 | `url_decode(text)` | dmasm |
| `0x109` | `Md5` | 1 -> 1 | `md5(text)` | dmasm |
| `0x10A` | `Text2Path` | 1 -> 1 | `text2path(text)`. Text containing `/proc/` or `/verb/` resolves to a proc, other text starting with `/` to a type, anything else to null | dmasm |
| `0x10B` | `WinOutput` | 3 -> 0 | `target << output(text, control)`. Stack is `target, text, control` | dm |
| `0x10C` | `WinSet` | 3 -> 0 | `winset(player, control, params)` | dm |
| `0x10D` | `WinGet` | 3 -> 1 | `winget(player, control, params)`. Parks the proc once the query is sent | dmasm |
| `0x10E` | `WinClone` | 3 -> 0 | `winclone(player, window, clone_name)` | dm |
| `0x10F` | `WinShow` | 3 -> 0 | `winshow(player, window, show)` | dm |
| `0x110` | `IconMapColors(arg_count: u32, var: Variable)` | n+1 -> 1 | `icon.MapColors()`. BYOND's own `/icon/proc/MapColors` pushes the icon and then 4, 5, 12 or 20 arguments, and the count leaves the icon out, unlike `IconInsert`. The stack effect is taken from those pushes, not from the handler. Writes the icon in `var`, and had the same missing operand in `dmasm` as `IconInsert` until 2026-10-04 | asm |
| `0x111` | `IconScale(var: Variable)` | 3 -> 1 | `icon.Scale(width, height)`. Stack is `icon, width, height`. Also writes the icon in `var`; `/icon/proc/Scale` pops the pushed value | dm |
| `0x112` | `IconCrop(var: Variable)` | 5 -> 1 | `icon.Crop(x1, y1, x2, y2)`. Stack is `icon, x1, y1, x2, y2`. Also writes the icon in `var`; `/icon/proc/Crop` pops the pushed value | dm |
| `0x113` | `Rgba` | 4 -> 1 | `rgb(r, g, b, a)` | dm |
| `0x114` | `IconStatesMode` | 2 -> 1 | `icon_states(icon, mode)`. `/icon/proc/IconStates` uses it too, with a null mode swapped for 0 | dm |
| `0x115` | `IconGetPixel(arg_count: u32)` | n -> 1 | `icon.GetPixel(x, y, icon_state, dir, frame, moving)`. `/icon/proc/GetPixel` pushes the icon and those six, count 7 | dm |
| `0x116` | `CallLib(arg_count: u32)` | n+2 -> 1 | `call_ext(lib, func)(args)`. Stack is `lib, func, arg0..argN-1`. A `byond,await:` call parks the proc and resumes with `lib` still under the result | read |
| `0x117` | `CallLibArgList` | 3 -> 1 | `call_ext(lib, func)(arglist(L))`. Leaks one reference per list item per call | read |
| `0x118` | `WinExists` | 2 -> 1 | `winexists(player, control)`. Parks the proc once the query is sent | dmasm |
| `0x119` | `IconBlend(var: Variable)` | 4 -> 0 | `icon.Blend(other, function, x, y)`. Stack is `other, function, x, y`, as BYOND's own `/icon/proc/Blend` pushes them. Blends into the icon held in `var` and writes it back | asm |
| `0x11A` | `IconSize` | 2 -> 1 | `icon.Width()` and `icon.Height()`. Stack is `icon, which`, where `which` is 1 for width and 2 for height | dm |
| `0x11B` | `Bounds(arg_count: u32)` | n -> 1 | `bounds(...)`, up to five arguments. Shares one handler with `OBounds` on 516.1669 | dmasm |
| `0x11C` | `OBounds(arg_count: u32)` | n -> 1 | `obounds(...)` | dmasm |
| `0x11D` | `BoundsDist` | 2 -> 1 | `bounds_dist(ref, target)` | dmasm |
| `0x11E` | `StepSpeed` | 3 -> 0, flag | `step(ref, dir, speed)` | dm |
| `0x11F` | `StepToSpeed` | 4 -> 0, flag | `step_to(ref, target, min, speed)` | dm |
| `0x120` | `StepAwaySpeed` | 4 -> 0, flag | `step_away(ref, target, max, speed)` | dm |
| `0x121` | `StepTowardsSpeed` | 3 -> 0, flag | `step_towards(ref, target, speed)` | dm |
| `0x122` | `StepRandSpeed` | 2 -> 0, flag | `step_rand(ref, speed)` | dm |
| `0x123` | `WalkSpeed` | 4 -> 0 | `walk(ref, dir, lag, speed)` | dm |
| `0x124` | `WalkToSpeed` | 5 -> 0 | `walk_to(ref, target, min, lag, speed)` | dm |
| `0x125` | `WalkAwaySpeed` | 5 -> 0 | `walk_away(ref, target, max, lag, speed)` | dm |
| `0x126` | `WalkTowardsSpeed` | 4 -> 0 | `walk_towards(ref, target, lag, speed)` | dm |
| `0x127` | `WalkRandSpeed` | 3 -> 0 | `walk_rand(ref, lag, speed)` | dm |
| `0x128` | `Animate` | 1 -> 1 | `animate(...)` with at least one change. Takes one associative list built by `NewAssocList` just before it. The object, when there is one, rides in that list under the key `1`; the chained form `animate(alpha = b)` has no such entry. As a statement the compiler pops the result | dm |
| `0x129` | `NullAnimate` | 1 -> 1 | `animate(object)` with an object and no changes. The name is misleading: the object is its one argument | dm |
| `0x12A` | `MatrixNew(arg_count: u32)` | n -> 1 | `matrix(...)`, up to six arguments | dmasm |
| `0x12B` | `Database(arg_count: u32)` | n -> 1 | Every database operation, not only creating one. All of BYOND's own `/database` and `/database/query` procs (`Open`, `Close`, `Error`, `Add`, `Execute`, `NextRow`, `GetRowData` and the rest) push `src` first, then one or two more values, and emit `Database 2` or `Database 3`. Which value picks the operation was not worked out | dm |
| `0x12C` | `Try(unk0: Label)` | 0 -> 0 | Opens a `try`. Pushes a catch frame recording the stack depth and the label. A throw inside the block lands on the label with the thrown value on the stack | read |
| `0x12D` | `Throw` | 1 -> 0 | `throw x` | dm |
| `0x12E` | `Catch(unk0: Label)` | 0 -> 0 | Leaves a `try` block normally. Jumps to the label and frees every catch frame whose range the label falls outside. It is a jump, not the start of the handler | asm |
| `0x12F` | `TryJmp(destination: Label)` | 0 -> 0 | Same jump-and-free as `Catch`, then counts down the loop budget like `JmpLoop` | asm |
| `0x130` | `ReplaceText` | 5 -> 1 | `replacetext(haystack, needle, replacement, start, end)`. Byte positions, Unicode case folding | jit |
| `0x131` | `ReplaceTextEx` | 5 -> 1 | `replacetextEx(...)`, case-sensitive | dmasm |
| `0x132` | `FindLastText` | 4 -> 1 | `findlasttext(haystack, needle, start, end)` | dmasm |
| `0x133` | `FindLastTextEx` | 4 -> 1 | `findlasttextEx(...)` | dmasm |
| `0x134` | `SpanText` | 3 -> 1 | `spantext(haystack, needles, start)` | dmasm |
| `0x135` | `NonSpanText` | 3 -> 1 | `nonspantext(haystack, needles, start)` | dmasm |
| `0x136` | `SplitText` | 5 -> 1 | `splittext(text, delimiter, start, end, include_delimiters)` | dmasm |
| `0x137` | `JoinText` | 4 -> 1 | `jointext(list, glue, start, end)`. Always four Values: the compiler pushes `1` and null for a missing start and end. A non-text glue joins with nothing | read |
| `0x138` | `JsonEncode` | 1 -> 1 | `json_encode(v)`. The result is always a string | read |
| `0x139` | `JsonDecode` | 1 -> 1 | `json_decode(text)`. Allows comments by default. A non-string argument fails inside the parser | read |
| `0x13A` | `RegexNew(arg_count: u32)` | n -> 1 | `regex(...)` | dmasm |
| `0x13B` | `FilterNewArgList` | 1 -> 1 | `filter(...)`. Takes one list: the associative list `NewAssocList` builds from the named arguments, or `L` itself for `filter(arglist(L))` | dm |
| `0x13C` | `PushTop` | 0 -> 1 | Pushes a second copy of the top of the stack | asm |
| `0x13D` | `SetCacheJmpIfNull(destination: Label)` | 1 -> 0, or stays on the jump | `a?.b`, for a read or a call. Writes the value into `cache` either way. A null also stays on the stack as the result and the jump is taken; anything else is popped | read |
| `0x13E` | `SetCachePopJmpIfNull(destination: Label)` | 1 -> 0 | `a?.b = v`, for a write. Null is discarded and the jump is taken. Anything else moves into `cache` | asm |
| `0x13F` | `PushEval` | 0 -> 1 | Pushes the result of the compound assignment just before it, for `(x += 1)` used as a value. It has no operand: the interpreter steps back and reads the **previous instruction's** `Variable` again. A resumed frame must not start on this instruction | read |
| `0x140` | `TestEquiv` | 2 -> 1, flag | `a ~= b`. Writes the flag **and** pushes the Number | asm |
| `0x141` | `TestNotEquiv` | 2 -> 1, flag | `a ~! b`. Same, with the answer inverted | asm |
| `0x142` | `PushCache` | 0 -> 0 | Saves `cache` into the operand stack at index 0, under every live operand. The stack size grows by one | read |
| `0x143` | `PopCache` | 0 -> 0 | Takes index 0 back into `cache` and releases what `cache` held | read |
| `0x144` | `Tan` | 1 -> 1 | `tan(x)`, in degrees | jit |
| `0x145` | `ArcTan` | 1 -> 1 | `arctan(x)`, result in degrees | jit |
| `0x146` | `ArcTan2` | 2 -> 1 | `arctan(x, y)`. Stack is `x, y`. Computes `atan2(y, x)` | jit |
| `0x147` | `IsList` | 1 -> 1 | `islist(x)`. Accepts about 46 tags, the whole list family plus `/alist`. Not the same set as `istype(x, /list)` | read |
| `0x148` | `Ref` | 1 -> 1 | `ref(x)`. The text `[0x` + hex of `(tag << 24) \| data` + `]` | read |
| `0x149` | `IsMovable` | 1 -> 0, flag | `ismovable(x)`. Tag `0x02` or `0x03` and the atom must still exist | read |
| `0x14A` | `Clamp` | 3 -> 1 | `clamp(value, low, high)`. Stack is `value, low, high`. Swaps the bounds when low > high | jit |
| `0x14B` | `Sha1` | 1 -> 1 | `sha1(x)` | dmasm |
| `0x14C` | `Text2AsciiChar` | 2 -> 1 | `text2ascii_char(text, pos)` | dmasm |
| `0x14D` | `LengthChar` | 1 -> 1 | `length_char(text)`, counting characters instead of bytes | jit |
| `0x14E` | `CopyTextChar` | 3 -> 1 | `copytext_char(text, start, end)` | dmasm |
| `0x14F` | `FindTextChar` | 4 -> 1 | `findtext_char(...)`. Positions count characters | jit |
| `0x150` | `FindTextExChar` | 4 -> 1 | `findtextEx_char(...)` | dmasm |
| `0x151` | `ReplaceTextChar` | 5 -> 1 | `replacetext_char(...)` | jit |
| `0x152` | `ReplaceTextExChar` | 5 -> 1 | `replacetextEx_char(...)` | dmasm |
| `0x153` | `FindLastTextChar` | 4 -> 1 | `findlasttext_char(haystack, needle, start, end)`. Always four values: the compiler pushes 0 and 1 for a missing `start` and `end` | dm |
| `0x154` | `FindLastTextExChar` | 4 -> 1 | `findlasttextEx_char(haystack, needle, start, end)`, with the same two filled-in defaults | dm |
| `0x155` | `SpanTextChar` | 3 -> 1 | `spantext_char(...)` | dmasm |
| `0x156` | `NonSpanTextChar` | 3 -> 1 | `nonspantext_char(...)` | dmasm |
| `0x157` | `SplitTextChar` | 5 -> 1 | `splittext_char(...)` | dmasm |
| `0x158` | `Text2NumRadix` | 2 -> 1 | `text2num(text, radix)`. Stack is `text, radix` | jit |
| `0x159` | `Num2TextRadix` | 3 -> 1 | `num2text(n, digits, radix)` | dm |
| `0x15A` | `AssignInto(var: Variable)` | 1 -> 0 | `var := x`. Takes the value; `var` is the target. The handler ends in the shared variable store and can call an overload proc first; the rest is not read | dm |
| `0x15B` | `PushCacheKey` | 0 -> 0 | `PushCache` for the `cache_key` register | read |
| `0x15C` | `PopCacheKey` | 0 -> 0 | `PopCache` for the `cache_key` register | read |
| `0x15D` | `Time2TextTZ(arg_count: u32)` | n -> 1 | `time2text(t, format, timezone)`, and only that three-argument form | read |
| `0x15E` | `MakeGenerator(unk0: u32)` | n -> 1 | Builds the handle behind a `/generator` datum. `generator(a, b, c, d)` itself compiles to `new /generator(...)`; BYOND's own `/generator/New` then pushes four values, emits `MakeGenerator 4` and stores the result in the datum's `_binobj` var | dm |
| `0x15F` | `SpliceText` | 4 -> 1 | `splicetext(text, start, end, insert)` | dmasm |
| `0x160` | `SpliceTextChar` | 4 -> 1 | `splicetext_char(...)` | dmasm |
| `0x161` | `RgbEx` | 5 -> 1 | `rgb()` when the compiler cannot prove the color space is plain RGB (`dmasm` source comment). For `rgb(a, b, c, space = d)` the stack is `a, b, c, null, d`: the null is the missing alpha | dm |
| `0x162` | `Rgb2Num` | 2 -> 1 | `rgb2num(color, space)` | dmasm |
| `0x163` | `GradientIndex` | 2 -> 1 | `gradient(list(...), index)`, where the gradient is already one value. Stack is `gradient, index`. Pushes the color as a `#rrggbb` string. Raises `bad gradient` when the first value is not a usable gradient. Added to `dmasm` on 2026-10-04 | asm |
| `0x164` | `Gradient` | 1 -> 1 | `gradient(...)` with the items written out. Takes one list holding the items with the index last, built by a `NewList` just before it, or by `NewAssocList` when arguments are named. `gradient(arglist(L))` passes `L` itself. Pushes the color as a `#rrggbb` string. `gradient(list(...), index)` compiles to `GradientIndex` instead | asm |
| `0x165` | `LoadResource(arg_count: u32)` | n -> 0 | `target << load_resource(...)`. The count includes the target. Does the send itself, so no `Output` follows | read |
| `0x166` | `IsPointer` | 1 -> 1 | True for tag `0x3C` only. No DM builtin is confirmed to produce it | read |
| `0x167` | `JsonEncodeFlags(arg_count: u32)` | n -> 1 | `json_encode(v, flags)`. The second Value is the flags. `dm.exe` only ever emits a count of 2. `JSON_STRICT` and `JSON_PRETTY_PRINT` are the same number, so passing either here pretty-prints | read |
| `0x168` | `JsonDecodeFlags(arg_count: u32)` | n -> 1 | `json_decode(text, flags)` | read |
| `0x169` | `Ceil` | 1 -> 1 | `ceil(x)` | jit |
| `0x16A` | `Trunc` | 1 -> 1 | `trunc(x)` | jit |
| `0x16B` | `Fract` | 1 -> 1 | `fract(x)`: `x - trunc(x)` | jit |
| `0x16C` | `IsNaN` | 1 -> 0, flag | `isnan(x)`. Flag only, like `IsMob` and unlike `IsNum`. A non-number is false | read |
| `0x16D` | `IsInf` | 1 -> 0, flag | `isinf(x)`. Flag only | read |
| `0x16E` | `TrimText` | 1 -> 1 | `trimtext(text)` | dm |
| `0x16F` | `FTime` | 2 -> 1 | `ftime(file, is_creation)`. Always two values: the compiler pushes null for a missing second argument | dm |
| `0x170` | `BlockXYZ` | 6 -> 1 | Six-argument `block()` used as an expression. Fills the shared scratch list and pushes a private copy (§ "The shared scratch list") | asm |
| `0x171` | `BlockXYZTarget` | 6 -> 1 | Six-argument `block()` written as the left side of `<<`. Pushes the shared scratch list itself, with no copy | asm |
| `0x172` | `NoiseHash(arg_count: u32)` | n -> 1 | `noise_hash(...)` with `arg_count` arguments | dm |
| `0x173` | `PowSquare` | 1 -> 1 | `x ** 2`, compiled as `x * x` | jit |
| `0x174` | `PowNegativeOne` | 1 -> 1 | `x ** -1`, compiled as `1 / x` | jit |
| `0x175` | `GetStepsTo` | 3 -> 1 | `get_steps_to(ref, target, min)` | dm |
| `0x176` | `FloatMod` | 2 -> 1 | `a %% b`: `a - trunc(a / b) * b`, without the integer truncation `%` does | jit |
| `0x177` | `AugFloatMod(var: Variable)` | 1 -> 0 | `var %%= x` | jit |
| `0x178` | `RefCount` | 1 -> 1 | `refcount(x)`. The internal count minus one. Always 0 for numbers, null, strings, turfs, the world and type paths | read |
| `0x179` | `LoadExt` | 2 -> 1 | `load_ext(lib, func)`. Pushes a handle (tag `0x45`) without calling anything | read |
| `0x17a` | `CallExtLoaded(arg_count: u32)` | n+1 -> 1 | `call_ext(handle)(args)`. Stack is `handle, arg0..argN-1` | read |
| `0x17b` | `CallExtLoadedArgList` | 2 -> 1 | `call_ext(handle)(arglist(L))`. Stack is `handle, list`. Copies the list's items into an argument array and calls the function `CallExtLoaded` calls. Leaks one reference per list item per call. Added to `dmasm` on 2026-10-04 | asm |
| `0x17c` | `NewAlist(arg_count: u32)` | 2n -> 1 | `alist(k = v, ...)`. Stack is one key and one value per pair, and `arg_count` counts pairs | dm |
| `0x17d` | `Spaceship` | 2 -> 1 | `a <=> b`. Calls an `operator<=>` overload when the left operand has one. The plain comparison path is not read | asm |
| `0x17e` | `KeyValueIter(var: Variable)` | 1 -> 0 | Part of `for(k, v in L)`. Takes the value `IterNext` pushed and stores it into `var`; a `SetVar` after it takes the key. If the loop was downgraded to a plain list walk it takes nothing and stores null | read |
| `0x17f` | `NewPixloc(arg_count: u32)` | n -> 1 | `pixloc(...)` with `arg_count` arguments. Seen with three | dm |
| `0x180` | `NewVector(arg_count: u32)` | n -> 1 | `vector(...)` with `arg_count` arguments. Seen with two and three | dm |
| `0x181` | `BoundPixloc` | 2 -> 1 | `bound_pixloc(atom, dir)` | dm |
| `0x182` | `Sin2` | 1 -> 1 | `sin(x)` again. 516 compilers emit either this or `Sin` | jit |
| `0x183` | `Cos2` | 1 -> 1 | `cos(x)` again | jit |
| `0x184` | `Tan2` | 1 -> 1 | `tan(x)` again | jit |
| `0x185` | `AsType` | 2 -> 1 | `astype(value, type)`. Stack is `value, type`. For one-argument `astype(value)` the compiler pushes the declared type of the var being assigned | dm |
| `0x186` | `Sign` | 1 -> 1 | `sign(x)` | jit |
| `0x187` | `Lerp` | 3 -> 1 | `lerp(a, b, factor)` | dm |
| `0x188` | `ValuesSum` | 1 -> 1 | `values_sum(L)`. Walks the associative values only | read |
| `0x189` | `ValuesProduct` | 1 -> 1 | `values_product(L)` | read |
| `0x18a` | `ValuesDot` | 2 -> 1 | `values_dot(A, B)` | read |
| `0x18b` | `ValuesCutUnder` | 3 -> 1 | `values_cut_under(L, min, inclusive)`. The compiler pushes null for a missing `inclusive`. Pushes the number removed. **Under is `0x18B`, over is `0x18C`** | read |
| `0x18c` | `ValuesCutOver` | 3 -> 1 | `values_cut_over(L, max, inclusive)` | read |
| `0x1337` | `AuxtoolsDebugBreak` | - | Not a BYOND opcode: it is outside `0x000..=0x18C`. An `auxtools` debugger extension | name |
| `0x1338` | `AuxtoolsDebugBreakNop` | - | Not a BYOND opcode. The second `auxtools` extension word | name |

### Operators

The arithmetic and bitwise rows above are short on purpose. Three rules cover
most surprises, and each was measured on a live world:

- **Null is not always 0.** `-`, `*`, `&`, `<<`, `>>`, `%` and `%%` read null
  as 0. `+`, `|` and `^` return the other operand untouched, so `null | 5.7`
  is `5.7` and `null + null` is null. `/` reads a null dividend as 0 and
  raises on a null divisor.
- **A list on the left changes the operator.** `+`, `-`, `&`, `|` and `^` each
  have a list form that builds a new list. The compound forms (`+=` and so on)
  change the list in place.
- **Bitwise operators work on 24 bits**, because that is how many whole-number
  bits an `f32` holds.

### Calls

Every call shape and where its pieces sit on the stack:

| DM | Instruction | Stack before |
|---|---|---|
| `f(a, b)` (global) | `CallGlob 2 /proc/f` | `a, b` |
| `f(arglist(L))`, `f(a = 1)` (global) | `CallGlobalArgList /proc/f` | `list` |
| `o.f(a, b)` | `Call cache = o; dynamic_proc("f") 2` | `a, b` |
| `o.f(arglist(L))`, `o.f(a = 1)` | `Call cache = o; dynamic_proc("f") 65535` | `list` |
| `o?.f(a)` | `SetCacheJmpIfNull`, then `Call dynamic_proc("f") 1` | `a` |
| `..()` | `CallParent` | nothing |
| `..(a, b)` | `CallParentArgs 2` | `a, b` |
| `..(arglist(L))` | `CallParentArgList` | `list` |
| `call(p)(a)` | `CallPath 1` | `p, a` |
| `call(o, "f")(a)` | `CallName 1` | `o, "f", a` |
| `call_ext(lib, "f")(a)` | `CallLib 1` | `lib, "f", a` |
| `call_ext(handle)(a)` | `CallExtLoaded 1` | `handle, a` |
| `new T(a, b)` | `New 2` | `T, a, b` |

A called proc always gets a frame padded to its declared parameter count.

### Loops

These are `dm.exe` 516.1687 output, disassembled with `dmasm` on 2026-10-04,
debug lines removed.

`for(var/x in L)`. A top-level loop has no `IterPush` or `IterPop`; they only
wrap a loop nested inside another one. The store comes **before** the `Jz`, so
the null that `IterNext` pushes at the end is stored too and the stack is
empty on both edges:

```text
          GetVar arg(0)
          IterLoad 5 ()
LAB_0017: IterNext
          SetVar local(1)
          Jz LAB_0027
          ...body...
          JmpLoop LAB_0017
LAB_0027:
```

`for(var/k, v in L)` uses kind 20, and `KeyValueIter` takes the value before
the `SetVar` takes the key:

```text
          GetVar arg(0)
          IterLoad 20 ()
LAB_0021: IterNext
          KeyValueIter local(2)
          SetVar local(1)
          Jz LAB_0034
          ...body...
          JmpLoop LAB_0021
LAB_0034:
```

`for(var/i in 1 to n)` keeps its counter and bound on the operand stack for
the whole loop:

```text
          PushInt 1
          GetVar arg(0)
          Check2Numbers
LAB_0017: ForRange LAB_0025 local(1)
          ...body...
          JmpLoop LAB_0017
LAB_0025: PopN 2
```

`for(var/i in 1 to n step 2)` pushes the step third and uses `Check3Numbers`,
`ForRangeStep` and `PopN 3`.

### Try and catch

Same source and date as the loops above. `Try`'s label is the start of the
catch block, and the thrown value is on the stack when execution lands there.
`Catch` sits at the end of the `try` body and jumps over the catch block:

```text
          Try LAB_0011
          ...try body...
          Catch LAB_0021
LAB_0011: SetVar local(0)
          ...catch body...
LAB_0021:
```

## The shared scratch list

The VM keeps one list with id 0 and reuses it instead of allocating: an
instruction empties it, fills it, and pushes a reference to list 0. That
reference only stays valid until the next instruction that refills the list.

- `View`, `OView`, `Block` and `BlockXYZ` fill it and swap in a private copy
  inside the same handler.
- `Range` and `ORange` push it uncopied, and the compiler puts `CopyList` after
  them.
- When the list is the left side of `<<`, as in `view() << "hi"`, the compiler
  skips the copy, presumably because `Output` uses the list up straight away.
  That is what `ViewTarget`, `OViewTarget`, `BlockTarget`, `BlockXYZTarget` and
  `NewListTarget` are for, and it is the only place found so far where `Range`
  and `ORange` appear with no `CopyList` behind them.

`viewers()`, `hearers()` and `newlist()` have no such form: on the left of
`<<` they compile to the same instruction as anywhere else.

## Opcode slots dmasm does not decode

`dmasm` stops with an unknown-opcode error on any of these.

No DM statement is known to make `dm.exe` 516.1687 emit them. The 2013
compiler (§ "The 2013 compiler") has no line that emits any of them directly
either, except
`0x26` and `0xCE` on a path nothing reaches. A full SS13 codebase (77,118
named procs) disassembled with no errors on 2026-09-12.

**Do not read "a station codebase reads clean" as proof a slot is dead.**
Twelve slots that used to be on this list were live, and `dmasm` could not
disassemble a proc holding any of them until it gained them on 2026-10-04:

| Slots | What emits them |
|---|---|
| `0x163`, `0x17B` | `gradient(list(...), index)` and `call_ext(handle)(arglist(L))` |
| `0x0A` | `target << key(file)` |
| `0x1D`, `0x1E`, `0x20`, `0x58`, `0x171` | `view()`, `oview()`, `block()` or `list()` as the left side of `<<` |
| `0xAE` | every `range()` and `orange()`. `dmasm` used to read it as an operand of `Range`, which broke on `range() << x` |
| `0xA3`, `0xA4` | the old `waitfor` block statement |
| `0xE0` | `_dm_icon_blend()` with three arguments |

What found them: the same builtin can compile to a different instruction
depending on where it sits, so a probe has to try it as a value, as a loop
source and as the left side of `<<`. The 2013 compiler's code is the fastest
way to learn which positions matter.

Every row below was read on 516.1687 Windows on 2026-10-04: the handler in
disassembly, and the functions it calls. Nothing here was run, so a row
describes what the code does, not a measured result.

Three rows (`0x26`, `0xCE`, `0xD5`) also lean on the 2013 Linux build
500.1205, which kept its function names and holds the DM compiler as well as
the interpreter (§ "The 2013 compiler"). Where a row says what that compiler
pushes, it was read from the compiler's code. Its handlers for these three
slots read the same stack slots as the 516.1687 ones.

| Op | Operands | Stack | What it does |
|---|---|---|---|
| `0x06` | none | 3 -> 0 | Old `target << sound(file, repeat)`. Stack is `target, file, repeat`. Builds a sound from the file, repeating when the number is not zero, at volume 100, and sends it. The current compiler builds a `/sound` datum and uses `Output` |
| `0x26` | `arg_count: u32` | n+2 -> 0 | A `spawn` that names a proc where its body would be: queues a new call of a proc value (tag `0x26`) to start after a delay. `usr` is copied from the running proc and `src` is null. Stack is `proc, delay, arg...`; the 2013 compiler pushes null for a missing delay. The handler takes the arguments from two slots above the proc, which matches that layout, but it reads the delay from the top of the stack. That is the `delay` slot only when there are no arguments; with arguments the last one is used as the delay too. No compiler on disk can emit it: the 2013 one holds the code for a `spawn` with no body block, but both it and 516.1687 give a bodyless `spawn(P)` an empty body and compile an ordinary `Spawn`, and both reject a second argument with `spawn: extra args` |
| `0x4F` | none | 0 -> 0, flag | Flips the test flag |
| `0x9C` | none | 0 -> 0 | Does nothing |
| `0x9D` | three words, as `Input` | as `Input` | A third way into the `input()` code. `Input` enters with mode 1, `InputColor` with 2, this one with 0. Mode 0 reads the same stack slots in a different order. Parks the proc like the other two |
| `0xBD` | none | 1 -> 1 | Replaces the value on top with null |
| `0xCE` | none | 3 -> 0 | `0x26` for `arglist(L)`. Stack is `proc, delay, list`. Passes the list as the one argument with the "arguments come from a list" flag set. The delay is read from the top of the stack here too, so it is always the list and never the `delay` slot. The 2013 compiler swaps `0x26` for this when the first argument after the delay is `arglist()`, on the same path nothing reaches |
| `0xD5` | `arg_count: u32` | n+1 -> 0 | Old `target << sound(file, repeat, wait, channel, volume)` taking 1 to 5 sound arguments. The file is the first of the `arg_count` values and `repeat`, `wait`, `channel`, `volume` follow it; a missing volume is 100. The handler reads the target from the `repeat` slot, not from the slot under the arguments, so `repeat` is used as the target as well; with one argument it reads one slot past the top of the stack. The 2013 handler does the same, and the 2013 compiler already has no code that emits this, `0x06` or `0xD6`. Not run |
| `0xD6` | none | 2 -> 0 | Old `target << S` for a `/sound` datum. Stack is `target, sound`. Reads five vars off the datum and sends them through the same code as `0xD5` |
| `0xD8` | none | 0 -> 0 | The bad-instruction slot. Raises `BYOND Error: bad instruction: <number>` and carries on |
## Older names

`dmasm`'s `src/opcodes_unused.rs` keeps an earlier name list, and extools uses
some of the same names. Where they differ from the current ones:

| Op | Older name | Current name |
|---|---|---|
| `0x2A` | `CallNoReturn` | `CallStatement` |
| `0xF8` `0xF9` `0xFA` | `Jmp2` `Jnz2` `Jz2` | `JmpLoop` `JnzLoop` `JzLoop` |
| `0xFC` | `CheckNum` | `Check2Numbers` |
| `0xFE` | `ForRangeStepSetup` | `Check3Numbers` |
| `0x13C` | `BeginListSetExpr` | `PushTop` |
| `0x13D` | `JmpIfNull` | `SetCacheJmpIfNull` |
| `0x13E` | `JmpIfNull2` | `SetCachePopJmpIfNull` |
| `0x13F` | `NullCacheMaybe` | `PushEval` |
| `0x142` `0x143` | `PushToCache` `PopFromCache` | `PushCache` `PopCache` |

`0x35` is `SetVarExpr` in both lists. extools calls it `SETVAR_COPY`.

## Keeping this file current

When `dmasm` gains or changes an instruction:

1. Check the operand count against the interpreter's handler for that opcode.
   An instruction that reads a word `dmasm` does not declare desyncs the
   disassembler from that point on: the missed word is read as the next
   opcode, and the error shows up an instruction or two later.
2. Add or fix the row here, copying the declaration from `src/instructions.rs`
   as written.
3. Give the row the weakest basis that is true.
4. Update the date at the top of this file.

To find out what DM source produces an instruction, write the construct into a
throwaway `.dme`, compile it with `dm.exe`, and disassemble the procs in the
resulting `.dmb`. An unknown-opcode error from that is how `0x163` and `0x17B`
turned out to be live, and the `dm` rows in the table came from the same loop.
Three things that make it work:

- Try each builtin in every position: as a value, as a loop source, and as the
  left side of `<<`. The position can change the instruction.
- Every compiled world also carries BYOND's own `/icon`, `/sound`,
  `/database`, `/regex` and `/generator` procs. Those bodies show the icon and
  database instructions with their real arguments.
- The compiler folds constants, so a probe built only from literals may
  compile to a single `PushInt`. Use proc arguments as the inputs.

### The 2013 compiler

When no probe produces a slot, the old compiler can say which statement emits
it. BYOND 500.1205 for Linux (2013) shipped a `libbyond.so` that kept its C++
function names, and that library holds the DM compiler as well as the
interpreter. Every opcode number compared so far means the same thing there
and in 516 (`0x07` to `0x0A`, `0x25`, `0x26`, `0xCE`, `0xD5`).

| What | Where in `libbyond.so` 500.1205 |
|---|---|
| Emit one bytecode word | `ProcCode::Add(ushort)`. Look for a constant passed as its argument |
| Emit a call instruction | `ProcCode::CallInst(Node*, ushort, Node*, Node*, ushort)`. The opcode is the second argument, and it swaps in the `arglist()` variant by itself |
| Statement and builtin dispatch | `ProcCode::EvalProcBlockInternal(Node*, ulong)`, one large switch on the statement's token id |
| Token names | An array of `char*` in the data section. Token id = index + 1 |

A constant that matches an opcode number is not always an opcode. `Add(6)`
after `Add(0xDF)` is the mode operand of `TurnOrFlipIcon`, not opcode `0x06`.

That is how `0x0A` turned out to be `target << key(file)`, how the `Target`
instructions were traced to the left side of `<<`, and how `0xA3`/`0xA4` were
traced to the `waitfor` block. The `DreamMaker` from the same build still runs
on Linux and rejects the same `spawn` spellings 516 does.
