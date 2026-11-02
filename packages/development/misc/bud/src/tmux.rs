//! A thin layer over the tmux CLI.

use anyhow::{bail, Context, Result};
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

pub fn run(args: &[&str]) -> Result<String> {
    let out = Command::new("tmux")
        .args(args)
        .stdin(Stdio::null())
        .output()
        .context("running tmux")?;
    if !out.status.success() {
        let err = String::from_utf8_lossy(&out.stderr);
        bail!("tmux {}: {}", args.first().unwrap_or(&""), err.trim());
    }
    Ok(String::from_utf8_lossy(&out.stdout).into_owned())
}

pub fn run_with_input(args: &[&str], input: &str) -> Result<()> {
    let mut child = Command::new("tmux")
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context("running tmux")?;
    child.stdin.take().unwrap().write_all(input.as_bytes())?;
    let out = child.wait_with_output()?;
    if !out.status.success() {
        bail!("tmux {}: {}", args[0], String::from_utf8_lossy(&out.stderr).trim());
    }
    Ok(())
}

pub fn has_session(name: &str) -> bool {
    Command::new("tmux")
        .args(["has-session", "-t", &format!("={name}")])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

/// `display-message -p` of `fmt` for `target`, or None if tmux fails.
pub fn display(target: &str, fmt: &str) -> Option<String> {
    run(&["display-message", "-p", "-t", target, fmt])
        .ok()
        .map(|s| s.trim_end_matches('\n').to_string())
}

/// The pane this process runs in, if it runs inside tmux.
pub fn current_pane() -> Option<String> {
    std::env::var("TMUX").ok()?;
    std::env::var("TMUX_PANE").ok().filter(|p| !p.is_empty())
}

/// Separates fields in tmux formats. Without a UTF-8 locale, tmux prints a
/// tab or any other control character as `_`.
pub const SEP: &str = "<|>";

pub struct Session {
    pub created: u64,
    pub attached: bool,
    pub activity: u64,
    /// The working directory of the session's active pane.
    pub path: String,
}

pub struct Pane {
    pub session: String,
    pub id: String,
    pub dead: bool,
    pub dead_status: i32,
    pub state: String,
    pub pid: u32,
}

/// All sessions and panes of the server, read once.
#[derive(Default)]
pub struct Snapshot {
    pub sessions: HashMap<String, Session>,
    pub panes: Vec<Pane>,
}

impl Snapshot {
    pub fn read() -> Snapshot {
        let mut snap = Snapshot::default();
        let fmt = ["#{session_name}", "#{session_created}", "#{session_attached}", "#{session_activity}", "#{pane_current_path}"].join(SEP);
        // No server running means no sessions.
        let Ok(out) = run(&["list-sessions", "-F", &fmt]) else { return snap };
        for line in out.lines() {
            let f: Vec<&str> = line.split(SEP).collect();
            if f.len() != 5 {
                continue;
            }
            snap.sessions.insert(
                f[0].to_string(),
                Session {
                    created: f[1].parse().unwrap_or(0),
                    attached: f[2].parse::<u32>().unwrap_or(0) > 0,
                    activity: f[3].parse().unwrap_or(0),
                    path: f[4].to_string(),
                },
            );
        }
        let fmt = ["#{session_name}", "#{pane_id}", "#{pane_dead}", "#{pane_dead_status}", "#{@agent_state}", "#{pane_pid}"].join(SEP);
        if let Ok(out) = run(&["list-panes", "-a", "-F", &fmt]) {
            for line in out.lines() {
                let f: Vec<&str> = line.split(SEP).collect();
                if f.len() != 6 {
                    continue;
                }
                snap.panes.push(Pane {
                    session: f[0].to_string(),
                    id: f[1].to_string(),
                    dead: f[2] == "1",
                    dead_status: f[3].parse().unwrap_or(0),
                    state: f[4].to_string(),
                    pid: f[5].parse().unwrap_or(0),
                });
            }
        }
        snap
    }

    pub fn pane(&self, session: &str, id: &str) -> Option<&Pane> {
        self.panes.iter().find(|p| p.session == session && p.id == id)
    }

    pub fn session_panes(&self, session: &str) -> Vec<&Pane> {
        self.panes.iter().filter(|p| p.session == session).collect()
    }

    pub fn session_of_pane(&self, id: &str) -> Option<&str> {
        self.panes.iter().find(|p| p.id == id).map(|p| p.session.as_str())
    }
}

/// The last `lines` nonblank lines of a pane's screen.
pub fn capture(pane: &str, lines: usize) -> Vec<String> {
    let start = format!("-{}", (lines * 4).max(200));
    let text = run(&["capture-pane", "-p", "-J", "-t", pane, "-S", &start]).unwrap_or_default();
    let all: Vec<String> = text
        .lines()
        .map(|l| l.trim_end().to_string())
        .filter(|l| !l.trim().is_empty())
        .collect();
    all[all.len().saturating_sub(lines)..].to_vec()
}

/// Startup and confirmation screens seen in agent TUIs. Typed text or Enter
/// there picks a choice instead of reaching the agent. A miss only means
/// `send` cannot refuse on that screen.
const DIALOG_MARKERS: &[&str] = &[
    "Enter to confirm",
    "enter confirm",
    "Esc to cancel",
    "Trust this folder",
    "Trust project folder",
    "Do you trust",
    "Hooks need review",
    "Update Available",
];

pub fn dialog_open(pane: &str) -> bool {
    capture(pane, 15)
        .iter()
        .any(|l| DIALOG_MARKERS.iter().any(|m| l.contains(m)))
}
