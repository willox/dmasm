use crate::operands::Operand;
use crate::parser;
use crate::{
    assembler::{AssembleEnv, AssembleError, Assembler},
    disassembler::{DebugData, DisassembleEnv, DisassembleError, Disassembler},
    list_operands::*,
    operands::*,
};
use std::fmt;

#[allow(unused)]
macro_rules! instructions {
    ( $(
        $opcode:literal = $name:ident
        $( ( $(
            $operand_name:ident: $operand_type:tt
        ),* $(,)? ) )?
    ),* $(,)? ) => {
        #[derive(PartialEq, Debug)]
        pub enum Instruction {
            $(
                $name$( ( $( $operand_type, )* ) )?,
            )*
        }

        impl Instruction {
            pub fn assemble<'a, E: AssembleEnv>(&'a self, asm: &mut Assembler<'a, E>) -> Result<(), AssembleError> {
                match self {
                    $(
                        Self::$name$( ( $( $operand_name, )* ) )? => {
                            asm.emit($opcode);
                            $( $( $operand_name.assemble(asm)?; )* )?
                        }
                    )*
                }

                Ok(())
            }

            pub fn disassemble<'a, E: DisassembleEnv>(
                dism: &mut Disassembler<'a, E>,
            ) -> Result<(Self, DebugData<'a>), DisassembleError> {
                let offset = dism.current_offset;

                let ins = match dism.read_u32()? {
                    $(
                        $opcode => {
                            Self::$name$( ( $( $operand_type::disassemble(dism)?, )* ) )?
                        }
                    )*

                    opcode => return Err(DisassembleError::UnknownOpcode { offset, opcode }),
                };

                let range_start = offset as usize;
                let range_end = dism.current_offset as usize;

                Ok((
                    ins,
                    DebugData {
                        offset,
                        bytecode: &dism.bytecode[range_start..range_end],
                    },
                ))
            }

            pub fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Self::$name$( ( $( $operand_name, )* ) )? => {
                            write!(f, stringify!($name))?;
                            $( $(
                                write!(f, " ")?;
                                $operand_name.serialize(f)?;
                            )* )?
                        }
                    )*
                }

                Ok(())
            }

            pub fn deserialize<'b, E: 'b>(i: &'b str) -> nom::IResult<&str, Self, E>
            where
                E: nom::error::ParseError<&'b str>
                    + nom::error::FromExternalError<&'b str, std::num::ParseIntError>,
            {
                let (i, name) = parser::whitespace(parser::parse_identifier)(i)?;

                let (i, instruction) = match name {
                    $(
                        stringify!($name) => {
                            $( $(
                                let (i, $operand_name) = parser::whitespace($operand_type::deserialize)(i)?;
                            )* )?
                            (i, Self::$name$( ( $(
                                $operand_name,
                             )* ))? )
                        },
                    )*

                    // TODO: Real error
                    other => panic!("unknown instruction {}", other),
                };

                Ok((i, instruction))
            }
        }
    };
}

instructions! {
    0x00 = End,
    0x01 = New(arg_count: u32),
    0x02 = Format(pattern: DMString, arg_count: u32),
    0x03 = Output,
    0x04 = OutputFormat(pattern: DMString, arg_count: u32),
    0x05 = Stat,
    // 0x06
    0x07 = Link,
    0x08 = OutputFtp,
    0x09 = OutputRun,
    // 0x0A
    // 0x0B
    0x0C = Del,
    0x0D = Test,
    0x0E = Not,
    0x0F = Jmp(destination: Label),
    0x11 = Jz(destination: Label),
    0x12 = Ret,
    0x13 = IsLoc,
    0x14 = IsMob,
    0x15 = IsObj,
    0x16 = IsArea,
    0x17 = IsTurf,
    0x18 = Alert,
    0x19 = EmptyList,
    0x1A = NewList(arg_count: u32),
    0x1B = View,
    0x1C = OView,
    // 0x1D
    // 0x1E
    0x1F = Block,
    0x21 = Prob,
    0x22 = Rand,
    0x23 = RandRange,
    0x24 = Sleep,
    0x25 = Spawn(destination: Label),
    // 0x26
    0x27 = BrowseRsc,
    0x28 = IsIcon,
    0x29 = Call(proc: Variable, arg_count: u32),
    0x2A = CallStatement(proc: Variable, arg_count: u32),
    0x2B = CallPath(arg_count: u32),
    0x2C = CallParent,
    0x2D = CallParentArgs(arg_count: u32),
    0x2E = CallSelf,
    0x2F = CallSelfArgs(arg_count: u32),
    0x30 = CallGlob(arg_count: u32, proc: Proc),
    0x31 = Log10,
    0x32 = Log,
    0x33 = GetVar(var: Variable),
    0x34 = SetVar(var: Variable),
    0x35 = SetVarExpr(var: Variable),
    0x36 = GetFlag,
    0x37 = Teq,
    0x38 = Tne,
    0x39 = Tl,
    0x3A = Tg,
    0x3B = Tle,
    0x3C = Tge,
    0x3D = UnaryNeg,
    0x3E = Add,
    0x3F = Sub,
    0x40 = Mul,
    0x41 = Div,
    0x42 = Mod,
    0x43 = Round,
    0x44 = RoundN,
    0x45 = AugAdd(var: Variable),
    0x46 = AugSub(var: Variable),
    0x47 = AugMul(var: Variable),
    0x48 = AugDiv(var: Variable),
    0x49 = AugMod(var: Variable),
    0x4A = AugBand(var: Variable),
    0x4B = AugBor(var: Variable),
    0x4C = AugXor(var: Variable),
    0x4D = AugLShift(var: Variable),
    0x4E = AugRShift(var: Variable),
    0x50 = PushInt(value: i32),
    0x51 = Pop,
    0x52 = IterLoad(unk0: u32, types: TypeFilter),
    0x53 = IterNext,
    0x54 = IterPush,
    0x55 = IterPop,
    0x56 = Num2TextSigFigs,
    0x57 = Roll,
    // 0x58
    0x59 = Range(params: RangeParams),
    0x5A = LocatePos,
    0x5B = LocateRef,
    0x5C = Flick,
    0x5D = Shutdown,
    0x5E = Startup(arg_count: u32),
    0x5F = RollStr,
    0x60 = PushVal(value: Value),
    0x61 = NewImage,
    0x62 = PreInc(var: Variable),
    0x63 = PostInc(var: Variable),
    0x64 = PreDec(var: Variable),
    0x65 = PostDec(var: Variable),
    0x66 = Inc(var: Variable),
    0x67 = Dec(var: Variable),
    0x68 = Abs,
    0x69 = Sqrt,
    0x6A = Pow,
    0x6B = Turn,
    0x6C = AddText(arg_count: u32),
    0x6D = Length,
    0x6E = CopyText,
    0x6F = FindText,
    0x70 = FindTextEx,
    0x71 = CmpText,
    0x72 = SortText(arg_count: u32),
    0x73 = SortTextEx(arg_count: u32),
    0x74 = UpperText,
    0x75 = LowerText,
    0x76 = Text2Num,
    0x77 = Num2Text,
    0x78 = Switch(params: SwitchParams),
    0x79 = PickSwitch(params: PickSwitchParams),
    0x7A = SwitchRange(params: SwitchRangeParams),
    0x7B = ListGet,
    0x7C = ListSet,
    0x7D = IsType,
    0x7E = Band,
    0x7F = Bor,
    0x80 = Bxor,
    0x81 = Bnot,
    0x82 = LShift,
    0x83 = RShift,
    0x84 = DbgFile(name: DMString),
    0x85 = DbgLine(line: u32),
    0x86 = Step,
    0x87 = StepTo,
    0x88 = StepAway,
    0x89 = StepTowards,
    0x8A = StepRand,
    0x8B = Walk,
    0x8C = WalkTo,
    0x8D = WalkAway,
    0x8E = WalkTowards,
    0x8F = WalkRand,
    0x90 = GetStep,
    0x91 = GetStepTo,
    0x92 = GetStepAway,
    0x93 = GetStepTowards,
    0x94 = GetStepRand,
    0x95 = GetDist,
    0x96 = GetDir,
    0x97 = LocateType,
    0x98 = Shell,
    0x99 = Text2File,
    0x9A = File2Text,
    0x9B = FCopy,
    // 0x9C
    // 0x9D
    0x9E = IsNull,
    0x9F = IsNum,
    0xA0 = IsText,
    0xA1 = StatPanel,
    0xA2 = StatPanelCheck,
    // 0xA3
    // 0xA4
    0xA5 = Min(arg_count: u32),
    0xA6 = Max(arg_count: u32),
    0xA7 = TypesOf(arg_count: u32),
    0xA8 = CKey,
    0xA9 = IsIn(params: IsInParams),
    0xAA = Browse,
    0xAB = BrowseOpt,
    0xAC = FList,
    0xAD = ORange(params: RangeParams),
    // 0xAE
    0xAF = Read,
    0xB0 = Index,
    0xB1 = PickProb(params: PickProbParams),
    0xB2 = JmpOr(destination: Label),
    0xB3 = JmpAnd(destination: Label),
    0xB4 = FDel,
    0xB5 = CallName(arg_count: u32),
    0xB6 = ShutdownArgs,
    0xB7 = List2Params,
    0xB8 = Params2List,
    0xB9 = CKeyEx,
    0xBA = PromptCheck,
    0xBB = Rgb,
    0xBC = HasCall,
    // 0xBD
    0xBE = HtmlEncode,
    0xBF = HtmlDecode,
    0xC0 = Time2Text,
    0xC1 = Input(unk0: u32, unk1: u32, unk2: u32),
    0xC2 = Sin,
    0xC3 = Cos,
    0xC4 = ArcSin,
    0xC5 = ArcCos,
    0xC6 = InputColor(unk0: u32, unk1: u32, unk2: u32),
    0xC7 = Crash,
    0xC8 = NewAssocList(arg_count: u32),
    0xC9 = CallParentArgList,
    0xCA = CallSelfArgList,
    0xCB = CallPathArgList,
    0xCC = CallNameArgList, // TODO: same as above but without a src?
    0xCD = CallGlobalArgList(proc: Proc),
    // 0xCE
    0xCF = NewArgList,
    0xD0 = MinList,
    0xD1 = MaxList,
    0xD2 = Pick,
    0xD3 = NewImageArgList,
    0xD4 = NewImageArgs(arg_count: u32),
    // 0xD5
    // 0xD6
    0xD7 = FCopyRsc,

    // This instruction must have been removed at some point
    // It's identical to running an invalid opcode
    // 0xD8 = BadInstruction,

    0xD9 = ShellAllowed,
    0xDA = RandSeed,
    0xDB = Text2Ascii,
    0xDC = Ascii2Text,
    0xDD = IconStates,
    0xDE = IconNew(arg_count: u32),
    0xDF = TurnOrFlipIcon(filter_mode: u32, var: Variable),
    // 0xE0
    0xE1 = IconIntensity(var: Variable),
    0xE2 = IconSwapColor(var: Variable),
    0xE3 = ShiftIcon(var: Variable),
    0xE4 = IsFile,
    0xE5 = Viewers,
    0xE6 = OViewers,
    0xE7 = Hearers,
    0xE8 = OHearers,
    0xE9 = DbNewConnection,
    // 0xEA
    0xEA = DbNewQuery,
    0xEB = DbConnect,
    0xEC = DbExecute,
    0xED = DbNextRow,
    0xEE = DbErrorMsg,
    0xEF = DbClose,
    0xF0 = DbIsConnected,
    0xF1 = DbRowsAffected,
    0xF2 = DbRowCount,
    0xF3 = DbQuote,
    0xF4 = DbColumns,
    0xF5 = IsPath,
    0xF6 = IsSubPath,
    0xF7 = FExists,

    // These are the same as Jmp, Jnz, and Jz, except they try to detect infinite loops
    0xF8 = JmpLoop(destination: Label),
    0xF9 = JnzLoop(destination: Label),
    0xFA = JzLoop(destination: Label),

    0xFB = PopN(count: u32),
    0xFC = Check2Numbers,
    0xFD = ForRange(exit: Label, var: Variable),
    0xFE = Check3Numbers,
    0xFF = ForRangeStep(exit: Label, var: Variable),
    0x100 = DmsNewKernel,
    0x101 = DmsParse,
    0x102 = DmsExportText,
    0x103 = Eval,
    0x104 = DmsPrepare,
    0x105 = IconDrawBox(var: Variable),
    0x106 = IconInsert(arg_count: u32),
    0x107 = UrlEncode,
    0x108 = UrlDecode,
    0x109 = Md5,
    0x10A = Text2Path,
    0x10B = WinOutput,
    0x10C = WinSet,
    0x10D = WinGet,
    0x10E = WinClone,
    0x10F = WinShow,
    0x110 = IconMapColors(arg_count: u32),
    0x111 = IconScale(var: Variable),
    0x112 = IconCrop(var: Variable),
    0x113 = Rgba,
    0x114 = IconStatesMode,
    0x115 = IconGetPixel(arg_count: u32),
    0x116 = CallLib(arg_count: u32),
    0x117 = CallLibArgList,
    0x118 = WinExists,
    0x119 = IconBlend(var: Variable),
    0x11A = IconSize,
    0x11B = Bounds(arg_count: u32),
    0x11C = OBounds(arg_count: u32),
    0x11D = BoundsDist,
    0x11E = StepSpeed,
    0x11F = StepToSpeed,
    0x120 = StepAwaySpeed,
    0x121 = StepTowardsSpeed,
    0x122 = StepRandSpeed,
    0x123 = WalkSpeed,
    0x124 = WalkToSpeed,
    0x125 = WalkAwaySpeed,
    0x126 = WalkTowardsSpeed,
    0x127 = WalkRandSpeed,
    0x128 = Animate,
    0x129 = NullAnimate,
    0x12A = MatrixNew(arg_count: u32),
    0x12B = Database(arg_count: u32),
    0x12C = Try(unk0: Label),
    0x12D = Throw,
    0x12E = Catch(unk0: Label),

    // TODO: Why does this exist?
    0x12F = TryJmp(destination: Label),

    0x130 = ReplaceText,
    0x131 = ReplaceTextEx,
    0x132 = FindLastText,
    0x133 = FindLastTextEx,
    0x134 = SpanText,
    0x135 = NonSpanText,
    0x136 = SplitText,
    0x137 = JoinText,
    0x138 = JsonEncode,
    0x139 = JsonDecode,
    0x13A = RegexNew(arg_count: u32),
    0x13B = FilterNewArgList,

    // Pushes the value at the top of the stack (it clones it)
    0x13C = PushTop,

    // Pops the value _only_ if the jump is performed
    // wait that might be backwards...
    0x13D = SetCacheJmpIfNull(destination: Label),

    // Pops the value regardless of if the jump is performed
    0x13E = JmpIfNull2(destination: Label),

    // This one's a bit of a beast. It seems that instructions such as AugAdd cause the offset of their variable operand to be cached
    // This instruction then uses the operand at that offset to find what the "last" var was and push it.
    // I haven't confirmed if anything but Aug* instructions set this value yet.
    0x13F = PushEval,

    0x140 = TestEquiv,
    0x141 = TestNotEquiv,

    // These instructions interact with a separate stack than the other ones (although both stacks share the same memory allocation.)
    // They push and pop the contents of the cache register
    0x142 = PushCache,
    0x143 = PopCache,

    0x144 = Tan,
    0x145 = ArcTan,
    0x146 = ArcTan2,
    0x147 = IsList,
    0x148 = Ref,
    0x149 = IsMovable,
    0x14A = Clamp,
    0x14B = Sha1,
    0x14C = Text2AsciiChar,
    0x14D = LengthChar,
    0x14E = CopyTextChar,
    0x14F = FindTextChar,
    0x150 = FindTextExChar,
    0x151 = ReplaceTextChar,
    0x152 = ReplaceTextExChar,
    0x153 = FindLastTextChar,
    0x154 = FindLastTextExChar,
    0x155 = SpanTextChar,
    0x156 = NonSpanTextChar,
    0x157 = SplitTextChar,
    0x158 = Text2NumRadix,
    0x159 = Num2TextRadix,
    // 0x15A
    // 0x15B
    // 0x15C
    0x15D = Time2TextTZ(arg_count: u32),
    // 0x15E
    0x15F = SpliceText,
    0x160 = SpliceTextChar,
    // 0x161
    0x162 = Rgb2Num, // This is technically a replacement for the original Rgb2Num which is somewhere else

    0x1337 = AuxtoolsDebugBreak,
    0x1338 = AuxtoolsDebugBreakNop,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.serialize(f)
    }
}
