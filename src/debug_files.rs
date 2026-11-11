//! Resolve DreamMaker debug source names from include paths.
//!
//! Checked on Windows and Linux, 515.1637–516.1688:
//! - A `.dme` include uses its literal include path with forward slashes.
//!   `_std/__std.dme` including `tgs.dm` names `_std/tgs.dm` as `tgs.dm`.
//! - Other includes are relative to the including file's debug-name directory,
//!   retaining `..` segments, e.g. `deep/../b.dm`.
//! - The root `.dme` retains its build-time path.
//!
//! Debug names can therefore differ from project-relative paths.
//! `__FILE__` uses project-relative paths instead.

use std::collections::HashSet;
use std::path::{Component, Path, PathBuf};

/// Return (debug name, normalized project-relative path) pairs in include
/// order, excluding `dme` itself. Follow literal include directives regardless
/// of `#if`; `keep` filters paths and traversal to files actually parsed.
pub fn names(dme: &Path, keep: &dyn Fn(&Path) -> bool) -> Vec<(String, PathBuf)> {
    let root = dme.parent().unwrap_or(Path::new(""));
    let mut found = vec![];
    let mut seen = HashSet::new();
    if let Some(file) = dme.file_name() {
        seen.insert(PathBuf::from(file));
        scan(root, Path::new(file), "", keep, &mut seen, &mut found);
    }
    found
}

/// Normalize `.` and `name/..` path components lexically.
pub fn normalize(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for part in path.components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir
                if matches!(out.components().next_back(), Some(Component::Normal(_))) =>
            {
                out.pop();
            }
            other => out.push(other),
        }
    }
    out
}

fn scan(
    root: &Path,
    file: &Path,
    prefix: &str,
    keep: &dyn Fn(&Path) -> bool,
    seen: &mut HashSet<PathBuf>,
    found: &mut Vec<(String, PathBuf)>,
) {
    let Ok(text) = std::fs::read(root.join(file)) else {
        return;
    };
    let text = String::from_utf8_lossy(&text);
    let directory = file.parent().unwrap_or(Path::new(""));
    for line in text.lines() {
        let Some(included) = included(line) else {
            continue;
        };
        let included = included.replace('\\', "/");
        let lower = included.to_ascii_lowercase();
        let environment = lower.ends_with(".dme");
        if !environment && !lower.ends_with(".dm") {
            continue;
        }
        let path = normalize(&directory.join(&included));
        if !keep(&path) || !root.join(&path).is_file() || !seen.insert(path.clone()) {
            continue;
        }
        let name = format!("{prefix}{included}");
        // What a file includes is named from where its own name is, but for
        // an environment's, which start over
        let inner = match name.rfind('/') {
            Some(slash) if !environment => name[..=slash].to_owned(),
            _ => String::new(),
        };
        found.push((name, path.clone()));
        scan(root, &path, &inner, keep, seen, found);
    }
}

/// The file an `#include "file"` line names
fn included(line: &str) -> Option<&str> {
    let rest = line
        .trim_start()
        .strip_prefix('#')?
        .trim_start()
        .strip_prefix("include")?;
    let rest = rest.trim_start().strip_prefix('"')?;
    Some(&rest[..rest.find('"')?])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn include_lines() {
        assert_eq!(
            included("#include \"code\\a.dm\" // first"),
            Some("code\\a.dm")
        );
        assert_eq!(included("  # include \"a.dm\""), Some("a.dm"));
        assert_eq!(included("#include <lib/lib.dme>"), None);
        assert_eq!(included("// #include \"a.dm\""), None);
        assert_eq!(included("#define A \"a.dm\""), None);
    }

    #[test]
    fn paths_without_dots() {
        assert_eq!(
            normalize(Path::new("sub/deep/../b.dm")),
            Path::new("sub/b.dm")
        );
        assert_eq!(normalize(Path::new("./a/./b.dm")), Path::new("a/b.dm"));
        assert_eq!(normalize(Path::new("../a.dm")), Path::new("../a.dm"));
        assert_eq!(normalize(Path::new("a/../../b.dm")), Path::new("../b.dm"));
    }

    /// The names DreamMaker 516.1688 and 515.1637 gave this tree's files
    #[test]
    fn names_as_dream_maker_has_them() {
        let dir = std::env::temp_dir().join(format!("dmasm-debug-files-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let write = |path: &str, text: &str| {
            let path = dir.join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        };
        write(
            "t.dme",
            "#include \"sub\\a.dme\"\n#include \"top.dm\"\n#include \"other/o.dm\"\n#include \"sub2/f.dm\"\n\
             #include \"gone.dm\"\n#include \"map.dmm\"\n#include \"top.dm\"\n",
        );
        write(
            "sub/a.dme",
            "#include \"b.dm\"\n#include \"x\\c.dm\"\n#include \"../other/d.dm\"\n#include \"deep/e.dm\"\n#include \"x/h.dme\"\n",
        );
        write("sub/b.dm", "");
        write("sub/x/c.dm", "#include \"c2.dm\"\n");
        write("sub/x/c2.dm", "");
        write("other/d.dm", "");
        write(
            "sub/deep/e.dm",
            "#include \"..\\b2.dm\"\n#include \"../../other/o2.dm\"\n",
        );
        write("sub/b2.dm", "");
        write("other/o2.dm", "");
        write("sub/x/h.dme", "#include \"c3.dm\"\n");
        write("sub/x/c3.dm", "");
        write("top.dm", "");
        write("other/o.dm", "");
        write("sub2/f.dm", "#include \"g.dm\"\n");
        write("sub2/g.dm", "");
        write("map.dmm", "");

        let all = names(&dir.join("t.dme"), &|_| true);
        let all: Vec<(&str, &Path)> = all
            .iter()
            .map(|(name, path)| (name.as_str(), path.as_path()))
            .collect();
        // Compared as paths: Windows has them with its own separator
        assert_eq!(
            all,
            [
                ("sub/a.dme", "sub/a.dme"),
                ("b.dm", "sub/b.dm"),
                ("x/c.dm", "sub/x/c.dm"),
                ("x/c2.dm", "sub/x/c2.dm"),
                ("../other/d.dm", "other/d.dm"),
                ("deep/e.dm", "sub/deep/e.dm"),
                ("deep/../b2.dm", "sub/b2.dm"),
                ("deep/../../other/o2.dm", "other/o2.dm"),
                ("x/h.dme", "sub/x/h.dme"),
                ("c3.dm", "sub/x/c3.dm"),
                ("top.dm", "top.dm"),
                ("other/o.dm", "other/o.dm"),
                ("sub2/f.dm", "sub2/f.dm"),
                ("sub2/g.dm", "sub2/g.dm"),
            ]
            .map(|(name, path)| (name, Path::new(path)))
        );

        let some = names(&dir.join("t.dme"), &|path| !path.starts_with("sub"));
        let some: Vec<&str> = some.iter().map(|(name, _)| name.as_str()).collect();
        assert_eq!(some, ["top.dm", "other/o.dm", "sub2/f.dm", "sub2/g.dm"]);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
