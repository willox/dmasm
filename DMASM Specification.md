(none of this is actually realised)

# DMASM
A fully documented assembly language that can be losslessly compiled to DM bytecode and back.

A DMASM document currently only represents a single proc (and thus contains no proc definitions - just the inner code.)

# State
(TODO)

Details of the VM's state go here. Uh, there's a stack. a cache register, some registers for the iterator state. What else?

# Syntax
(TODO)
- A DMASM document is encoded with valid UTF-8
- Lines starting with `;` are comments (they can start mid-line too)
- Lines which are an identifier followed by `:` are labels.
- All other lines are whitespace or instructions

## Operands
Operands are like function parameters. Instructions expect a specific set of operands (and sometimes none at all.) that specifies what they should do.

| Operand | Example | Description |
| ------- | ------- | ----------- |
| SInt | `5` or `0x6` | 32-bit signed integer. Simple enough. |
| UInt | `5u` or `0x6u` | 32-bit unsigned integer. |
| Label | `MyLabelName` | Reference to another instruction. |
| String | `"Hello, World!"` | UTF-8 encoded string. There are some escape sequences available (TODO). |
| Value | `Value(0xFF, 0xFFFFFFFF)` | Hardcoded internal DM value. |
| Proc | `/proc/DoStuff` | Reference to a proc. It could even be to the current proc. |
| Variable | See [Variable Operand](#Variable-Operand) | Reference to a variable. |
| Switch | See [Switch Operands](#Switch-Operands) | Settings for performing a switch statement. |
| SwitchPick | See [Switch Operands](#Switch-Operands) | Settings for performing a switch statement. |
| SwitchRange | See [Switch Operands](#Switch-Operands) | Settings for performing a switch statement. |

### Variable Operand
TODO: Too complex. Skipped for now.

### Switch Operands
TODO: Too complex. Skipped for now.

## Instructions
| Instruction | Syntax | Description |
| ----------- | ------- | ----------- |
| End | `End` | Returns the contents of the DOT variable from the current function. |
| New | `New <ArgCount: UInt>` | Pushes a new object using data that was pushed to the stack (TODO.) |
| Format | `Format <FormatStr: String> <ArgCount: UInt>` | Pushes a new string using the given format string and values from the stack. |
| Output | `Output` | Outputs to an object using the two values at the top of the stack. |
| OutputFormat | `OutputFormat <FormatStr: String> <ArgCount: UInt>` | Combines the behaviour of `Format` and `Output`. |
| Read | `Read` | TODO |
| Stat | `Stat` | TODO |
| Link | `Link` | TODO |
| OutputFtp | `OutputFtp` | TODO |
| OutputRun | `OutputRun` | TODO |
| !!! Missile | `Missile` | TODO |
| Del | `Del` | Clears all existing references to the value at the top of the stack. |
| Test | `Test` | TODO |
| Not | `Not` | TODO |
| Jmp | `Jmp <Destination: Label>` | TODO |
| !!!Jnz | `Jnz ???` | TODO |
| Jz | `Jz <Destination: Label>` | TODO |
| Ret | `Ret` | Returns the value at the top of the stack from the current function. |
| IsLoc | `IsLoc` | TODO |
| IsMob | `IsMob` | TODO |
| IsObj | `IsObj` | TODO |
| IsArea | `IsArea` | TODO |
| IsTurf | `IsTurf` | TODO |
| Alert | `Alert` | TODO |
| EmptyList | `EmptyList` | TODO |
| NewList | `NewList <Count: UInt>` | TODO |
| View | `View` | TODO |
| OView | `OView` | TODO |
| !!!ViewTarget | `ViewTarget??` | TODO |
| !!!OViewTarget | `OViewTarget??` | TODO |
| Block | `Block` | TODO |
| Prob | `Prob` | TODO |
| Rand | `Rand` | TODO |
| RandRange | `RandRange` | TODO |
| Sleep | `Sleep` | TODO |
| Spawn | `Spawn <Destination: Label>` | TODO |
| BrowseRsc | `BrowseRsc` | TODO |
| IsIcon | `IsIcon` | TODO |
| Call | `Call <Proc: Variable> <ArgCount: UInt>` | TODO |
| CallNoReturn | `CallNoReturn <Proc: Variable> <ArgCount: UInt>` | TODO |
| CallPath | `CallPath <ArgCount: UInt>` | TODO |
| CallParent | `CallParent` | TODO |
| CallParentArgList | `CallParentArgList` | TODO |
| CallParentArgs | `CallParentArgs <ArgCount: UInt>` | TODO |
| CallSelf | `CallSelf` | TODO |
| CallSelfArgList | `CallSelfArgList` | TODO |
| CallSelfArgs | `CallSelfArgs <ArgCount: UInt>` | TODO |
| CallGlob | `CallGlob <Proc> <ArgCount: UInt>` | TODO |
| Log10 | `Log10` | TODO |
| Log | `Log` | TODO |
| GetVar | `GetVar <Variable>` | TODO |
| SetVar | `SetVar <Variable>` | TODO |
| SetVarNoPop | `SetVarNoPop <Variable>` | TODO |
| GetFlag | `GetFlag` | TODO |
| Teq | `Teq` | TODO |
| Tne | `Tne` | TODO |
| Tl | `Tl` | TODO |
| Tg | `Tg` | TODO |
| Tle | `Tle` | TODO |
| Tge | `Tge` | TODO |
| TestNotEquiv | `TestNotEquiv` | TODO |
| UnaryNeg | `UnaryNeg` | TODO |
| Add | `Add` | TODO |
| Sub | `Sub` | TODO |
| Mul | `Mul` | TODO |
| Div | `Div` | TODO |
| Mod | `Mod` | TODO |
| Round | `Round` | TODO |
| RoundN | `RoundN` | TODO |
| AugAdd | `AugAdd <Variable>` | TODO |
| AugSub | `AugSub <Variable>` | TODO |
| AugMul | `AugMul <Variable>` | TODO |
| AugDiv | `AugDiv <Variable>` | TODO |
| AugMod | `AugMod <Variable>` | TODO |
| AugBand | `AugBand <Variable>` | TODO |
| AugBor | `AugBor <Variable>` | TODO |
| AugXor | `AugXor <Variable>` | TODO |
| AugLShift | `AugLShift <Variable>` | TODO |
| AugRShift | `AugRShift <Variable>` | TODO |
| PushInt | `PushInt <SInt>` | TODO |
| Pop | `Pop` | TODO |
| IterLoad | `IterLoad <UInt> <UInt>` | TODO |
| IterNext | `IterNext` | TODO |
| IterPush | `IterPush` | TODO |
| IterPop | `IterPop` | TODO |
| !!!Roll | `Roll???` | TODO |
| LocatePos | `LocatePos` | TODO |
| LocateRef | `LocateRef` | TODO |
| Flick | `Flick` | TODO |
| !!!Shutdown | `Shutdown???` | TODO |
| !!!Startup | `Startup???` | TODO |
| RollStr | `RollStr` | TODO |
| PushVal | `PushVal <Value>` | TODO |
| NewImage | `NewImage` | TODO |
| PreInc | `PreInc <Variable>` | TODO |
| PostInc | `PostInc <Variable>` | TODO |
| PreDec | `PreDec <Variable>` | TODO |
| PostDec | `PostDec <Variable>` | TODO |
| Inc | `Inc <Variable>` | TODO |
| Dec | `Dec <Variable>` | TODO |
| Abs | `Abs` | TODO |
| Sqrt | `Sqrt` | TODO |
| Pow | `Pow` | TODO |
| Turn | `Turn` | TODO |
| !!!AddText | `AddText??` | TODO |
| Length | `Length` | TODO |
| CopyText | `CopyText` | TODO |
| FindText | `FindText` | TODO |
| FindTextEx | `FindTextEx` | TODO |
| CmpText | `CmpText` | TODO |
| SortText | `SortText <ArgCount: UInt>` | TODO |
| SortTextEx | `SortTextEx <ArgCount: UInt>` | TODO |
| UpperText | `UpperText` | TODO |
| LowerText | `LowerText` | TODO |
| Ascii2Text | `Ascii2Text` | TODO |
| Text2Ascii | `Text2Ascii` | TODO |
| Text2Num | `Text2Num` | TODO |
| Num2Text | `Num2Text` | TODO |
| Num2TextSigFigs | `Num2TextSigFigs` | TODO |
| Switch | `Switch <Switch>` | TODO |
| SwitchPick | `SwitchPick <SwitchPick>` | TODO |
| SwitchRange | `SwitchRange <SwitchRange>` | TODO |
| ListGet | `ListGet` | TODO |
| ListSet | `ListSet` | TODO |
| BeginListSetExpr | `BeginListSetExpr` | TODO |
| IsType | `IsType` | TODO |
| Band | `Band` | TODO |
| Bor | `Bor` | TODO |
| Bxor | `Bxor` | TODO |
| Bnot | `Bnot` | TODO |
| LShift | `LShift` | TODO |
| RShift | `RShift` | TODO |
| DbgFile | `DbgFile <String>` | TODO |
| DbgLine | `DbgLine <UInt>` | TODO |
| Step | `Step` | TODO |
| StepTo | `StepTo` | TODO |
| StepAway | `StepAway` | TODO |
| StepTowards | `StepTowards` | TODO |
| StepRand | `StepRand` | TODO |
| StepSpeed | `StepSpeed` | TODO |
| StepToSpeed | `StepToSpeed` | TODO |
| Walk | `Walk` | TODO |
| WalkTo | `WalkTo` | TODO |
| WalkAway | `WalkAway` | TODO |
| WalkTowards | `WalkTowards` | TODO |
| WalkRand | `WalkRand` | TODO |
| GetStep | `GetStep` | TODO |
| GetStepTo | `GetStepTo` | TODO |
| GetStepAway | `GetStepAway` | TODO |
| GetStepTowards | `GetStepTowards` | TODO |
| GetStepRand | `GetStepRand` | TODO |
| GetDist | `GetDist` | TODO |
| GetDir | `GetDir` | TODO |
| LocateType | `LocateType` | TODO |
| Shell | `Shell` | TODO |
| Text2File | `Text2File` | TODO |
| File2Text | `File2Text` | TODO |
| FCopy | `FCopy` | TODO |
| IsNull | `IsNull` | TODO |
| IsNum | `IsNum` | TODO |
| IsText | `IsText` | TODO |
| !!!StatPanel | `StatPanel??` | TODO |
| !!!StatPanelCheck | `StatPanelCheck??` | TODO |
| Min | `Min <ArgCount: UInt>` | TODO |
| Max | `Max <ArgCount: UInt>` | TODO |
| TypesOf | `TypesOf <ArgCount: UInt>` | TODO |
| CKey | `CKey` | TODO |
| In | `In` | TODO |
| InRange | `InRange` | TODO |
| !!!Browse | `Browse??` | TODO |
| BrowseOpt | `BrowseOpt` | TODO |
| FList | `FList` | TODO |
| Index | `Index` | TODO |
| JmpOr | `JmpOr <Label>` | TODO |
| JmpAnd | `JmpAnd <Label>` | TODO |
| FDel | `FDel` | TODO |
| CallName | `CallName <ArgCount: UInt>` | TODO |
| List2Params | `List2Params` | TODO |
| Params2List | `Params2List` | TODO |
| CKeyEx | `CKeyEx` | TODO |
| PromptCheck | `PromptCheck` | TODO |
| Rgb | `Rgb` | TODO |
| Rgba | `Rgba` | TODO |
| HasCall | `HasCall` | TODO |
| HtmlEncode | `HtmlEncode` | TODO |
| HtmlDecode | `HtmlDecode` | TODO |
| Time2Text | `Time2Text` | TODO |
| Input | `Input <UInt> <UInt> <UInt>` | TODO |
| InputColor | `Input <UInt> <UInt> <UInt>` | TODO |
| Sin | `Sin` | TODO |
| Cos | `Cos` | TODO |
| ArcSin | `ArcSin` | TODO |
| ArcCos | `ArcCos` | TODO |
| Crash | `Crash` | TODO |
| NewAssocList | `NewAssocList <ArgCount: UInt>` | TODO |
| CallPathArgList | `CallPathArgList` | TODO |
| CallNameArgList | `CallNameArgList` | TODO |
| CallGlobalArgList | `CallGlobalArgList <Proc>` | TODO |
| NewArgList | `NewArgList` | TODO |
| MinList | `MinList` | TODO |
| MaxList | `MaxList` | TODO |
| Pick | `Pick` | TODO |
| PickProb | `PickProb <Label>, ...` | TODO |
| NewImageArgList | `NewImageArgList` | TODO |
| NewImageArgs | `NewImageArgs <ArgCount: UInt>` | TODO |
| FCopyRsc | `FCopyRsc` | TODO |
| RandSeed | `RandSeed` | TODO |
| IconStates | `IconStates` | TODO |
| IconNew | `IconNew <ArgCount: UInt>` | TODO |
| TurnOrFlipIcon | `TurnOrFlipIcon <Mode: UInt> <Variable>` | TODO |
| IconIntensity | `IconIntensity <Variable>` | TODO |
| IconBlend | `IconBlend <Variable>` | TODO |
| IconSwapColor | `IconSwapColor <Variable>` | TODO |
| IconDrawBox | `IconDrawBox <Variable>` | TODO |
| IconInsert | `IconInsert <ArgCount: UInt>` | TODO |
| IconMapColors | `IconMapColors <ArgCount: UInt>` | TODO |
| IconScale | `IconScale <Variable>` | TODO |
| IconCrop | `IconCrop <Variable>` | TODO |
| IconGetPixel | `IconGetPixel(ParamCount), <ArgCount: UInt>` | TODO |
| IconSize | `IconSize` | TODO |
| ShiftIcon | `ShiftIcon <Variable>` | TODO |
| IsFile | `IsFile` | TODO |
| Viewers | `Viewers` | TODO |
| OViewers | `OViewers` | TODO |
| Hearers | `Hearers` | TODO |
| OHearers | `OHearers` | TODO |
| IsPath | `IsPath` | TODO |
| IsSubPath | `IsSubPath` | TODO |
| FExists | `FExists` | TODO |
| Jmp2 | `Jmp2 <Label>` | TODO |
| Jnz2 | `Jnz2 <Label>` | TODO |
| Jz2 | `Jz2 <Label>` | TODO |
| PopN | `PopN <UInt>` | TODO |
| CheckNum | `CheckNum` | TODO |
| Range | `Range` | TODO |
| Orange | `Orange` | TODO |
| ForRange | `ForRange <Label> <Variable>` | TODO |
| ForRangeStepSetup | `ForRangeStepSetup` | TODO |
| ForRangeStep | `ForRangeStep <Label> <Variable>` | TODO |
| UrlEncode | `UrlEncode` | TODO |
| UrlDecode | `UrlDecode` | TODO |
| Md5 | `Md5` | TODO |
| Text2Path | `Text2Path` | TODO |
| WinOutput | `WinOutput` | TODO |
| WinSet | `WinSet` | TODO |
| WinGet | `WinGet` | TODO |
| WinClone | `WinClone` | TODO |
| WinShow | `WinShow` | TODO |
| IconStatesMode | `IconStatesMode` | TODO |
| CallLib | `CallLib <ArgCount: UInt>` | TODO |
| CallLibArgList | `CallLibArgList` | TODO |
| WinExists | `WinExists` | TODO |
| !!!Bounds | `Bounds??` | TODO |
| !!!OBounds | `OBounds??` | TODO |
| BoundsDist | `BoundsDist` | TODO |
| !!!StepAwaySpeed | `StepAwaySpeed??` | TODO |
| !!!StepTowardsSpeed | `StepTowardsSpeed??` | TODO |
| !!!StepRandSpeed | `StepRandSpeed??` | TODO |
| !!!WalkSpeed | `WalkSpeed??` | TODO |
| !!!WalkToSpeed | `WalkToSpeed??` | TODO |
| !!!WalkAwaySpeed | `WalkAwaySpeed??` | TODO |
| !!!WalkTowardsSpeed | `WalkTowardsSpeed??` | TODO |
| !!!WalkRandSpeed | `WalkRandSpeed??` | TODO |
| Animate | `Animate` | TODO |
| NullAnimate | `NullAnimate` | TODO |
| MatrixNew | `MatrixNew <ArgCount: UInt>` | TODO |
| Database | `Database <ArgCount: UInt>` | TODO |
| Try | `Try <Label>` | TODO |
| Throw | `Throw` | TODO |
| Catch | `Catch <Label>` | TODO |
| ReplaceText | `ReplaceText` | TODO |
| ReplaceTextEx | `ReplaceTextEx` | TODO |
| FindLastText | `FindLastText` | TODO |
| FindLastTextEx | `FindLastTextEx` | TODO |
| !!!SpanText | `SpanText??` | TODO |
| !!!NonSpanText | `NonSpanText??` | TODO |
| SplitText | `SplitText` | TODO |
| JoinText | `JoinText` | TODO |
| JsonEncode | `JsonEncode` | TODO |
| JsonDecode | `JsonDecode` | TODO |
| RegexNew | `RegexNew <ArgCount: UInt>` | TODO |
| FilterNewArgList | `FilterNewArgList` | TODO |
| JmpIfNull | `JmpIfNull <Label>` | TODO |
| JmpIfNull2 | `JmpIfNull2 <Label>` | TODO |
| NullCacheMaybe | `NullCacheMaybe` | TODO |
| PushToCache | `PushToCache` | TODO |
| PopFromCache | `PopFromCache` | TODO |
| Tan | `Tan` | TODO |
| ArcTan | `ArcTan` | TODO |
| ArcTan2 | `ArcTan2` | TODO |
| IsList | `IsList` | TODO |
| Ref | `Ref` | TODO |
| IsMovable | `IsMovable` | TODO |
| Clamp | `Clamp` | TODO |
| !!!Sha1 | `Sha1??` | TODO |
| LengthChar | `LengthChar` | TODO |
| FindTextChar | `FindTextChar` | TODO |
| CopyTextChar | `CopyTextChar` | TODO |
| ReplaceTextChar | `ReplaceTextChar` | TODO |
| ReplaceTextExChar | `ReplaceTextExChar` | TODO |
| FindLastTextChar | `FindLastTextChar` | TODO |
| FindLastTextExChar | `FindLastTextExChar` | TODO |
| SpanTextChar | `SpanTextChar` | TODO |
| NonSpanTextChar | `NonSpanTextChar` | TODO |
| !!!SplitTextChar | `SplitTextChar??` | TODO |
| Text2NumRadix | `Text2NumRadix` | TODO |
| Num2TextRadix | `Num2TextRadix` | TODO |
| DebugBreak | `DebugBreak` | TODO |
