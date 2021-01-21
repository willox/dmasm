#[derive(PartialEq, Debug)]
pub struct Label(pub String);

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Debug)]
pub struct Proc(pub String);

impl std::fmt::Display for Proc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// TODO: String Formatting
#[derive(PartialEq, Debug)]
pub struct DMString(pub String);

impl std::fmt::Display for DMString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Debug)]
pub enum Variable {
	Null,
	World,
	Usr,
	Src,
	Args,
	Dot,
    Cache,
    Cache2,
    Cache3,
	Unk1,
	Unk2,
    CurrentProc,
    Arg(u32),
    Local(u32),
    Global(DMString),
    Field(Box<Variable>, Vec<DMString>),
    Initial(Box<Variable>, Vec<DMString>),
    StaticProcField(Box<Variable>, Vec<DMString>, Proc),
    RuntimeProcField(Box<Variable>, Vec<DMString>, DMString),
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Null => write!(f, "null"),
            Variable::World => write!(f, "world"),
            Variable::Usr => write!(f, "usr"),
            Variable::Src => write!(f, "src"),
            Variable::Args => write!(f, "args"),
            Variable::Dot => write!(f, "dot"),
            Variable::Cache => write!(f, "cache"),
            Variable::Cache2 => write!(f, "cache2"),
            Variable::Cache3 => write!(f, "cache3"),
            Variable::Unk1 => write!(f, "unk1"),
            Variable::Unk2 => write!(f, "unk2"),
            Variable::CurrentProc => write!(f, "dotdot"),
            Variable::Arg(x) => write!(f, "arg({})", x),
            Variable::Local(x) => write!(f, "local({})", x),
            Variable::Global(name) => write!(f, "global({})", name),
            Variable::Field(var, fields) => write!(f, "field({} {})", **var, fields.iter().map(|x| x.0.clone()).collect::<Vec<String>>().join(" ")),
            Variable::Initial(var, fields) => write!(f, "initial({} {})", **var, fields.iter().map(|x| x.0.clone()).collect::<Vec<String>>().join(" ")),
            Variable::StaticProcField(var, fields, name) => write!(f, "static_proc({} {} {})", **var, fields.iter().map(|x| x.0.clone()).collect::<Vec<String>>().join(" "), name),
            Variable::RuntimeProcField(var, fields, name) => write!(f, "runtime_proc({} {} {})", **var, fields.iter().map(|x| x.0.clone()).collect::<Vec<String>>().join(" "), name),
        }
    }
}
