//! Compare incremental reload results and timing with a full project parse.
//! Temporarily insert a statement before an indented if in each FILE, then
//! restore the source.
//!
//! `cargo run --release --example check_file_reload DME FILE...`
use std::{collections::HashSet, path::PathBuf, time::Instant};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let dme = PathBuf::from(&args[0]);
    let root = dme.parent().unwrap().to_owned();
    let options = dmasm::reload::ParseOptions {
        defines: vec![],
        byond: Some((516, 1688)),
    };
    let t = Instant::now();
    let mut snapshot = dmasm::reload::Snapshot::parse(&dme, &options).unwrap();
    println!("full parse: {:?}", t.elapsed());
    for file in &args[1..] {
        let path = root.join(file);
        let original = std::fs::read_to_string(&path).unwrap();
        // Find an indented if statement as the insertion point.
        let mut lines: Vec<String> = original.lines().map(str::to_owned).collect();
        let Some(at) = lines.iter().position(|l| {
            l.starts_with('\t')
                && !l.trim_start().starts_with("//")
                && !l.trim().is_empty()
                && l.trim_start().starts_with("if")
        }) else {
            println!("{file}: no line to edit");
            continue;
        };
        let indent: String = lines[at].chars().take_while(|c| *c == '\t').collect();
        lines.insert(at, format!("{indent}world.log << \"hot reload check\""));
        std::fs::write(&path, lines.join("\n") + "\n").unwrap();

        let t = Instant::now();
        let quick = snapshot.reload_file(&root, std::path::Path::new(file));
        let quick_time = t.elapsed();
        let t = Instant::now();
        let new = dmasm::reload::Snapshot::parse(&dme, &options).unwrap();
        let changed: HashSet<PathBuf> = [PathBuf::from(file)].into();
        let full = dmasm::reload::diff(&snapshot, &new, Some(&changed));
        let full_time = t.elapsed();
        std::fs::write(&path, &original).unwrap();
        match quick {
            Err(e) => println!(
                "{file}: parsed on its own: no ({e}) in {quick_time:?}; full {full_time:?}"
            ),
            Ok(mut r) => {
                let mut full = full;
                r.changes.reloads.sort_by(|a, b| a.proc.cmp(&b.proc));
                full.reloads.sort_by(|a, b| a.proc.cmp(&b.proc));
                r.changes.line_moves.sort_by(|a, b| a.proc.cmp(&b.proc));
                full.line_moves.sort_by(|a, b| a.proc.cmp(&b.proc));
                let same_reloads = r.changes.reloads.len() == full.reloads.len()
                    && r.changes
                        .reloads
                        .iter()
                        .zip(&full.reloads)
                        .all(|(a, b)| a.proc == b.proc && a.nodes == b.nodes);
                let same_moves = r.changes.line_moves == full.line_moves;
                if !same_reloads {
                    println!(
                        "  on its own: {:?}",
                        r.changes
                            .reloads
                            .iter()
                            .map(|r| r.proc.to_string())
                            .collect::<Vec<_>>()
                    );
                    println!(
                        "  full:       {:?}",
                        full.reloads
                            .iter()
                            .map(|r| r.proc.to_string())
                            .collect::<Vec<_>>()
                    );
                }
                for (a, b) in r.changes.reloads.iter().zip(&full.reloads) {
                    if a.nodes != b.nodes {
                        let fa: String = a.nodes.iter().map(|n| format!("{:?}\n", n)).collect();
                        let fb: String = b.nodes.iter().map(|n| format!("{:?}\n", n)).collect();
                        for (x, y) in fa.lines().zip(fb.lines()) {
                            if x != y {
                                println!(
                                    "  {}: on its own {:?} / full {:?}",
                                    a.proc,
                                    x.trim(),
                                    y.trim()
                                );
                                break;
                            }
                        }
                    }
                }
                println!(
                    "{file}: on its own in {quick_time:?} ({} reloads, {} moved), full {full_time:?} ({} reloads, {} moved, {} refused): same reloads {same_reloads}, same moves {same_moves}",
                    r.changes.reloads.len(), r.changes.line_moves.len(), full.reloads.len(), full.line_moves.len(), full.refused.len()
                );
            }
        }
    }
}
