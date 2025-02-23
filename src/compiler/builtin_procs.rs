use dreammaker::ast::Expression;

use crate::compiler::*;
use crate::operands;
use crate::Instruction;

#[allow(non_camel_case_types)]
enum DefaultValue {
    null,
    Number(f32),
}

impl DefaultValue {
    fn into_value(self) -> operands::Value {
        match self {
            Self::null => operands::Value::Null,
            Self::Number(val) => operands::Value::Number(val),
        }
    }
}

impl From<i32> for DefaultValue {
    fn from(val: i32) -> Self {
        Self::Number(val as f32)
    }
}

impl From<f32> for DefaultValue {
    fn from(val: f32) -> Self {
        Self::Number(val)
    }
}

// # Simple-Stack Procs
// These are built-in procs where a fixed amount of values are pushed to the stack
// followed by a bare instruction (with no operands)
// The parameters can have default values. If any params are missing, it'll error.
// Note: Many of the defaults are null because that's how BYOND's compiler does it.
macro_rules! simple_stack_procs {
    (
        $(
            /proc/$proc_name:ident
            (
                $(
                    $param_name:ident
                    $(
                        = $param_default:tt
                    )?
                ),* $(,)?
            ) => $instruction:expr
        ),* $(,)?
    ) => {
        fn eval_simple_stack_procs(
            compiler: &mut Compiler<'_>,
            name: &str,
            args: &Vec<Expression>,
        ) -> Result<Option<EvalKind>, CompileError> {
            match name {
                $(
                    stringify!($proc_name) => {
                        #[allow(unused_mut)]
                        let mut arg_idx = 0;
                        let arg_count = args.len();

                        args::emit_normal(compiler, args::ArgsContext::Proc, args.to_owned())?;

                        $(
                            arg_idx += 1;

                            if arg_count < arg_idx {
                                #[allow(unused)]
                                let mut default: Option<DefaultValue> = None;

                                $(
                                    #[allow(unused_imports)]
                                    use DefaultValue::null;
                                    default = Some(DefaultValue::from($param_default));
                                )?

                                match default {
                                    Some(default) => {
                                        compiler.emit_ins(Instruction::PushVal(default.into_value()));
                                    }

                                    None => {
                                        return Err(CompileError::MissingArgument {
                                            proc: stringify!($proc_name).to_owned(),
                                            index: arg_idx as u32,
                                        });
                                    }
                                }
                            }
                        )*

                        if arg_count > arg_idx {
                            return Err(CompileError::TooManyArguments {
                                proc: stringify!($proc_name).to_owned(),
                                expected: arg_idx as u32,
                            });
                        }

                        compiler.emit_ins($instruction);
                        Ok(Some(EvalKind::Stack))
                    }
                )*

                _ => Ok(None),
            }
        }
    }
}

simple_stack_procs! {
    /proc/abs(val) => Instruction::Abs,
    /proc/alert(param_1, param_2 = null, param_3 = null, param_4 = null, param_5 = null, param_6 = null) => Instruction::Alert,
    /proc/arccos(val) => Instruction::ArcCos,
    /proc/arcsin(val) => Instruction::ArcSin,
    /proc/ascii2text(val) => Instruction::Ascii2Text,
    /proc/block(start, end) => Instruction::Block,
    /proc/bounds_dist(ref, target) => Instruction::BoundsDist,
    /proc/ckey(text) => Instruction::CKey,
    /proc/cKey(text) => Instruction::CKeyEx,
    /proc/ckeyEx(text) => Instruction::CKeyEx,
    /proc/clamp(val) => Instruction::Clamp,
    /proc/copytext(text, start, end = null) => Instruction::CopyText,
    /proc/copytext_char(text, start, end = null) => Instruction::CopyTextChar,
    /proc/cos(val) => Instruction::Cos,
    /proc/fcopy(src, dst) => Instruction::FCopy,
    /proc/fcopy_rsc(path) => Instruction::FCopyRsc,
    /proc/fdel(path) => Instruction::FDel,
    /proc/fexists(path) => Instruction::FExists,
    /proc/file2text(path) => Instruction::File2Text,
    /proc/findlasttext(haystack, needle, start = 0, end = 1) => Instruction::FindLastText,
    /proc/findlasttextEx(haystack, needle, start = 0, end = 1) => Instruction::FindLastTextEx,
    /proc/findtext(haystack, needle, start = 0, end = 1) => Instruction::FindText,
    /proc/findtext_char(haystack, needle, start = 0, end = 1) => Instruction::FindTextChar,
    /proc/findtextEx(haystack, needle, start = 0, end = 1) => Instruction::FindTextEx,
    /proc/findtextEx_char(haystack, needle, start = 0, end = 1) => Instruction::FindTextExChar,
    /proc/flist(path) => Instruction::FList,
    /proc/get_dir(loc1, loc2) => Instruction::GetDir,
    /proc/get_dist(loc1, loc2) => Instruction::GetDist,
    /proc/get_step(ref, dir) => Instruction::GetStep,
    /proc/get_step_away(ref, dir, max = null) => Instruction::GetStepAway,
    /proc/get_step_rand(ref) => Instruction::GetStepRand,
    /proc/get_step_to(ref, target, min = null) => Instruction::GetStepTo,
    /proc/get_step_towards(ref, target) => Instruction::GetStepTowards,
    /proc/hascall(obj, proc_name) => Instruction::HasCall,
    /proc/hearers(depth = null, center = null) => Instruction::Hearers,
    /proc/html_decode(text) => Instruction::HtmlDecode,
    /proc/html_encode(text) => Instruction::HtmlEncode,
    /proc/isfile(val) => Instruction::IsFile,
    /proc/isicon(val) => Instruction::IsIcon,
    /proc/islist(val) => Instruction::IsList,
    /proc/isnull(val) => Instruction::IsNull,
    /proc/isnum(val) => Instruction::IsNum,
    /proc/istext(val) => Instruction::IsText,
    /proc/istype(val, path) => Instruction::IsType,
    /proc/jointext(list, glue, start = 1, end = null) => Instruction::JoinText,
    /proc/json_decode(json) => Instruction::JsonDecode,
    /proc/json_encode(val) => Instruction::JsonEncode,
    /proc/length(val) => Instruction::Length,
    /proc/length_char(val) => Instruction::LengthChar,
    /proc/lentext(text) => Instruction::Length,
    /proc/list2params(list) => Instruction::List2Params,
    /proc/lowertext(text) => Instruction::LowerText,
    /proc/md5(text) => Instruction::Md5,
    /proc/nonspantext(haystack, needles, start = 1) => Instruction::NonSpanText,
    /proc/nonspantext_char(haystack, needles, start = 1) => Instruction::NonSpanTextChar,
    /proc/ohearers(depth = null, center = null) => Instruction::OHearers,
    /proc/orange(dist = null, center = null) => Instruction::ORange(operands::RangeParams),
    /proc/oview(dist = null, center = null) => Instruction::OView,
    /proc/oviewers(depth = null, center = null) => Instruction::OViewers,
    /proc/params2list(params) => Instruction::Params2List,
    /proc/prob(val) => Instruction::Prob,
    /proc/range(dist = null, center = null) => Instruction::Range(operands::RangeParams),
    /proc/ref(val) => Instruction::Ref,
    /proc/replacetext(haystack, needle, replacement, start = 1, end = null) => Instruction::ReplaceText,
    /proc/replacetext_char(haystack, needle, replacement, start = 1, end = null) => Instruction::ReplaceTextChar,
    /proc/replacetextEx(haystack, needle, replacement, start = 1, end = null) => Instruction::ReplaceTextEx,
    /proc/replacetextEx_char(haystack, needle, replacement, start = 1, end = null) => Instruction::ReplaceTextExChar,
    /proc/rgb2num(color, space = 0) => Instruction::Rgb2Num,
    /proc/sha1(arg) => Instruction::Sha1,
    /proc/sin(val) => Instruction::Sin,
    /proc/spantext(haystack, needles, start = 1) => Instruction::SpanText,
    /proc/spantext_char(haystack, needles, start = 1) => Instruction::SpanTextChar,
    /proc/splicetext(text, start, end = null, insert = null) => Instruction::SpliceText,
    /proc/splicetext_char(text, start, end = null, insert = null) => Instruction::SpliceTextChar,
    /proc/splittext(text, delimiter, start = 1, end = null, include_delimiters = null) => Instruction::SplitText,
    /proc/splittext_char(text, delimiter, start = 1, end = null, include_delimiters = null) => Instruction::SplitTextChar,
    /proc/sqrt(val) => Instruction::Sqrt,
    /proc/tan(val) => Instruction::Tan,
    /proc/text2ascii(text, pos = null) => Instruction::Text2Ascii,
    /proc/text2ascii_char(text, pos = null) => Instruction::Text2AsciiChar,
    /proc/text2file(text, file) => Instruction::Text2File,
    /proc/text2path(text) => Instruction::Text2Path,
    /proc/turn(var, angle) => Instruction::Turn,
    /proc/uppertext(text) => Instruction::UpperText,
    /proc/url_decode(text) => Instruction::UrlDecode,
    /proc/url_encode(text, format = null) => Instruction::UrlEncode,
    /proc/view(dist = null, center = null) => Instruction::View,
    /proc/winexists(player, control_id) => Instruction::WinExists,
    /proc/winget(player, control_id, params) => Instruction::WinGet,
    /proc/_dm_db_new_query() => Instruction::DbNewQuery,
    /proc/_dm_db_execute(a, b, c, d, e) => Instruction::DbExecute,
    /proc/_dm_db_next_row(a, b, c) => Instruction::DbNextRow,
    /proc/_dm_db_rows_affected(a) => Instruction::DbRowsAffected,
    /proc/_dm_db_row_count(a) => Instruction::DbRowCount,
    /proc/_dm_db_error_msg(a) => Instruction::DbErrorMsg,
    /proc/_dm_db_columns(a, b) => Instruction::DbColumns,
    /proc/_dm_db_close(a) => Instruction::DbClose,
    /proc/_dm_db_new_con() => Instruction::DbNewConnection,
    /proc/_dm_db_connect(a, b, c, d, e, f) => Instruction::DbConnect,
    /proc/_dm_db_quote(a, b) => Instruction::DbQuote,
    /proc/_dm_db_is_connected(a) => Instruction::DbIsConnected,
}

macro_rules! simple_vararg_procs {
    (
        $(
            /proc/$proc_name:ident
            (
                $($min_args:literal)?
                $(..= $max_args:literal)?
            ) => $instruction:expr
        ),* $(,)?
    ) => {
        fn eval_simple_vararg_procs(
            compiler: &mut Compiler<'_>,
            name: &str,
            args: &[Expression],
        ) -> Result<Option<EvalKind>, CompileError> {
            let args_len = args.len() as u32;

            match name {
                $(
                    stringify!($proc_name) => {
                        $(
                            if args_len < $min_args {
                                return Err(CompileError::IncorrectArgCount(name.to_owned()));
                            }
                        )?

                        $(
                            if args_len > $max_args {
                                return Err(CompileError::IncorrectArgCount(name.to_owned()));
                            }
                        )?

                        args::emit_normal(compiler, args::ArgsContext::Proc, args.to_vec())?;
                        compiler.emit_ins($instruction(args_len));
                        Ok(Some(EvalKind::Stack))
                    }
                )*

                _ => Ok(None),
            }
        }
    }
}

simple_vararg_procs! {
    /proc/addtext(2) => Instruction::AddText,
    /proc/bounds(..=5) => Instruction::Bounds,
    /proc/matrix(..=6) => Instruction::MatrixNew,
    /proc/max(1) => Instruction::Max,
    /proc/min(1) => Instruction::Min,
    /proc/obounds(..=5) => Instruction::OBounds,
    /proc/regex() => Instruction::RegexNew, // TODO: Byond doesn't error on arg count for regex(), but we could
    /proc/sorttext(2) => Instruction::SortText,
    /proc/sortText(2) => Instruction::SortTextEx,
    /proc/sorttextex(2) => Instruction::SortTextEx,
    /proc/startup(1) => Instruction::Startup,
    /proc/typesof(1) => Instruction::TypesOf,
}

// # Unsupported Procs
// Get to these later.
macro_rules! unsupported_procs {
    (
        $(
            /proc/$proc_name:ident
        ),* $(,)?
    ) => {
        fn is_unsupported_proc(
            name: &str,
        ) -> bool {
            match name {
                $(
                    stringify!($proc_name) => true,
                )*

                _ => false,
            }
        }
    }
}

unsupported_procs! {
    // Should be unreachable asthe parser turns these into their own terms
    /proc/list,
    /proc/locate,
    /proc/pick,
    /proc/input,

    // Overloaded Procs
    /proc/arctan,
    /proc/icon_states,
    /proc/ispath,
    /proc/log,
    /proc/num2text,
    /proc/rand,
    /proc/roll,
    /proc/round,
    /proc/shell,
    /proc/shutdown,
    /proc/text2num,

    // Snowflake procs
    /proc/animate,
    /proc/cmptext,
    /proc/cmptextEx,
    /proc/file,
    /proc/filter,
    /proc/gradient,
    /proc/icon,
    /proc/image,
    /proc/isarea,
    /proc/isloc,
    /proc/ismob,
    /proc/ismovable,
    /proc/isobj,
    /proc/issaved,
    /proc/isturf,
    /proc/newlist,
    /proc/rgb,
    /proc/sound,
    /proc/step,
    /proc/step_away,
    /proc/step_rand,
    /proc/step_to,
    /proc/step_towards,
    /proc/text,

    // Not actually procs
    /proc/browse,
    /proc/browse_rsc,
    /proc/flick,
    /proc/ftp,
    /proc/link,
    /proc/load_resource,
    /proc/output,
    /proc/rand_seed,
    /proc/run,
    /proc/sleep,
    /proc/stat,
    /proc/statpanel,
    /proc/walk,
    /proc/walk_away,
    /proc/walk_rand,
    /proc/walk_to,
    /proc/walk_towards,
    /proc/winclone,
    /proc/winset,
    /proc/winshow,
    /proc/CRASH,
}

pub(super) fn emit(
    compiler: &mut Compiler<'_>,
    name: &str,
    args: &Vec<Expression>,
) -> Result<Option<EvalKind>, CompileError> {
    if is_unsupported_proc(name) {
        return Err(CompileError::UnsupportedBuiltin {
            proc: name.to_owned(),
        });
    }

    if let Some(res) = eval_simple_stack_procs(compiler, name, args)? {
        return Ok(Some(res));
    }

    if let Some(res) = eval_simple_vararg_procs(compiler, name, args)? {
        return Ok(Some(res));
    }

    let arg_count = args.len() as u32;

    match name {
        "arglist" => {
            if arg_count != 1 {
                return Err(CompileError::IncorrectArgCount(name.to_owned()));
            }

            args::emit_single_normal(compiler, args::ArgsContext::Proc, args[0].clone())?;
            Ok(Some(EvalKind::ArgList))
        }

        "initial" => {
            if arg_count != 1 {
                return Err(CompileError::IncorrectArgCount(name.to_owned()));
            }

            let var = match compiler.emit_expr(args[0].clone())? {
                EvalKind::Field(builder, field) => {
                    builder.get_initial_field(DMString(field.into()))
                }

                _ => return Err(CompileError::ExpectedFieldReference),
            };

            // The chain builder can't handle the kind of var we have, so move the reuslt to the stack
            compiler.emit_ins(Instruction::GetVar(var));
            Ok(Some(EvalKind::Stack))
        }

        _ => Ok(None),
    }
}
