//! Processes started from tmux panes. These checks read /proc, so they run
//! on Linux only; elsewhere they report that nothing was checked.

use std::collections::{HashMap, HashSet};
use std::path::Path;

pub const SUPPORTED: bool = cfg!(target_os = "linux");

pub struct Proc {
    pub pid: u32,
    pub pane: String,
    pub cwd: String,
    pub args: String,
}

/// `pids` and every process below them.
pub fn descendants(pids: &[u32]) -> HashSet<u32> {
    let mut children: HashMap<u32, Vec<u32>> = HashMap::new();
    for (pid, dir) in proc_dirs() {
        let Ok(stat) = std::fs::read_to_string(dir.join("stat")) else { continue };
        let ppid = stat
            .rsplit_once(')')
            .and_then(|(_, rest)| rest.split_whitespace().nth(1))
            .and_then(|p| p.parse::<u32>().ok());
        if let Some(ppid) = ppid {
            children.entry(ppid).or_default().push(pid);
        }
    }
    let mut out = HashSet::new();
    let mut todo: Vec<u32> = pids.to_vec();
    while let Some(p) = todo.pop() {
        if out.insert(p) {
            todo.extend(children.get(&p).into_iter().flatten());
        }
    }
    out
}

pub fn alive(pid: u32) -> bool {
    Path::new(&format!("/proc/{pid}")).exists()
}

fn proc_dirs() -> impl Iterator<Item = (u32, std::path::PathBuf)> {
    std::fs::read_dir("/proc")
        .into_iter()
        .flatten()
        .flatten()
        .filter_map(|e| Some((e.file_name().to_str()?.parse::<u32>().ok()?, e.path())))
}

/// Processes of this user that inherited TMUX_PANE from the tmux server at `socket`.
fn pane_processes(socket: &str) -> Vec<Proc> {
    let me = std::process::id();
    let mut out = Vec::new();
    for (pid, dir) in proc_dirs() {
        if pid == me {
            continue;
        }
        let Ok(environ) = std::fs::read(dir.join("environ")) else { continue };
        let mut pane = None;
        let mut sock = None;
        for kv in environ.split(|b| *b == 0) {
            if let Some(v) = kv.strip_prefix(b"TMUX_PANE=") {
                pane = Some(String::from_utf8_lossy(v).into_owned());
            } else if let Some(v) = kv.strip_prefix(b"TMUX=") {
                let v = String::from_utf8_lossy(v);
                sock = v.split(',').next().map(str::to_string);
            }
        }
        let (Some(pane), Some(sock)) = (pane, sock) else { continue };
        if pane.is_empty() || sock != socket {
            continue;
        }
        let args = std::fs::read(dir.join("cmdline"))
            .map(|c| String::from_utf8_lossy(&c).replace('\0', " ").trim().to_string())
            .unwrap_or_default();
        let cwd = std::fs::read_link(dir.join("cwd"))
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        out.push(Proc { pid, pane, cwd, args });
    }
    out
}

/// Processes whose tmux pane is gone; only those from `panes` if given.
pub fn strays(socket: &str, live: &HashSet<String>, panes: Option<&HashSet<String>>) -> Vec<Proc> {
    pane_processes(socket)
        .into_iter()
        .filter(|p| !live.contains(&p.pane) && panes.is_none_or(|s| s.contains(&p.pane)))
        .collect()
}

pub fn print(procs: &[Proc]) {
    for p in procs {
        let args: String = p.args.chars().take(200).collect();
        println!("  pid {}  pane {}  cwd {}\n    {}", p.pid, p.pane, p.cwd, args);
    }
}
