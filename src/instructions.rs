use crate::operands::*;

#[derive(PartialEq, Debug)]
pub enum Instruction {
    End,
    Format(DMString, u32),
    Jmp(Label),
    Output,
    DbgFile(DMString),
    DbgLine(u32),
    PushInt(i32),
    Ret,
    GetVar(Variable),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::End => write!(f, "End"),
            Self::Format(string, arg_count) => write!(f, "Format {} {}", string, arg_count),
            Self::Jmp(label) => write!(f, "Jmp {}", label),
            Self::Output => write!(f, "Output"),
            Self::DbgFile(path) => write!(f, "DbgFile {}", path),
            Self::DbgLine(line) => write!(f, "DbgLine {}", line),
            Self::PushInt(v) => write!(f, "PushInt {}", v),
            Self::Ret => write!(f, "Ret"),
            Self::GetVar(var) => write!(f, "GetVar {}", var),
        }
    }
}
