//! Integration tests: each test runs its own tmux server with stub agents.

use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const BUD: &str = env!("CARGO_BIN_EXE_bud");

// Installed under every harness name. Records its argv, cwd and environment;
// reports a state the way the harness hooks do (except as agy); then logs
// every line typed into the pane.
const STUB: &str = r#"#!/bin/sh
log="$STUB_LOG/${TMUX_PANE#%}"
prog=$(basename "$0")
: > "$log.argv"
for a in "$@"; do printf '%s\0' "$a" >> "$log.argv"; done
pwd > "$log.cwd"
env > "$log.env"
case "$*" in *"EXIT 3"*) exit 3 ;; esac
case "$*" in *SPAWN*) setsid sleep 300 </dev/null >/dev/null 2>&1 & echo $! > "$log.spawned" ;; esac
case "$*" in *DIALOG*) printf ' Yes, I trust this folder\n Enter to confirm · Esc to cancel\n' ;; esac
sleep 0.3
[ "$prog" = agy ] || tmux set -p -t "$TMUX_PANE" @agent_state done
printf '\033[?2004h> '
while IFS= read -r line; do printf '%s\n' "$line" >> "$log.typed"; done
"#;

static COUNTER: AtomicUsize = AtomicUsize::new(0);

struct Env {
    dir: PathBuf,
    vars: Vec<(String, String)>,
    socket: String,
    server_pid: String,
    clients: Vec<Child>,
}

impl Env {
    fn new() -> Env {
        // tmux sockets have a short path limit.
        let tmp = std::env::var("TMPDIR").ok().filter(|t| t.len() < 40).unwrap_or("/tmp".into());
        let n = COUNTER.fetch_add(1, Ordering::SeqCst);
        let dir = PathBuf::from(tmp).join(format!("bud-{}-{n}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        for d in ["bin", "log", "home", "work"] {
            std::fs::create_dir_all(dir.join(d)).unwrap();
        }
        for name in ["claude", "codex", "pi", "opencode", "agy"] {
            let p = dir.join("bin").join(name);
            std::fs::write(&p, STUB).unwrap();
            Command::new("chmod").arg("755").arg(&p).status().unwrap();
        }
        let path = format!("{}:{}", dir.join("bin").display(), std::env::var("PATH").unwrap());
        let s = |p: &str| dir.join(p).display().to_string();
        let server_vars = vec![
            ("PATH".into(), path),
            ("HOME".into(), s("home")),
            ("TMUX_TMPDIR".into(), dir.display().to_string()),
            ("STUB_LOG".into(), s("log")),
            ("CLAUDECODE".into(), "1".into()),
            ("GIT_CONFIG_NOSYSTEM".into(), "1".into()),
            ("GIT_CONFIG_GLOBAL".into(), s("gitconfig")),
            ("TERM".into(), "xterm-256color".into()),
        ];
        std::fs::write(dir.join("gitconfig"), "[user]\nname = t\nemail = t@t\n[init]\ndefaultBranch = main\n").unwrap();
        let mut env = Env { dir: dir.clone(), vars: server_vars, socket: String::new(), server_pid: String::new(), clients: vec![] };
        // The server, and so every pane, lacks the state directory that bud
        // is given: panes must get it from bud.
        env.tmux(&["new-session", "-d", "-s", "keepalive", "-x", "200", "-y", "50", "sleep", "100000"]);
        env.tmux(&["set-option", "-g", "default-shell", "/bin/sh"]);
        env.socket = env.tmux(&["display-message", "-p", "#{socket_path}"]).trim().into();
        env.server_pid = env.tmux(&["display-message", "-p", "#{pid}"]).trim().into();
        env.vars.extend([
            ("BUD_STATE_DIR".into(), s("state")),
            ("BUD_SEND_DELAY".into(), "0.1".into()),
            ("BUD_CLOSE_GRACE".into(), "0.3".into()),
            ("BUD_IDLE_SECS".into(), "15".into()),
            ("BUD_IDLE_MAX".into(), "1".into()),
        ]);
        env
    }

    fn cmd(&self, program: &str) -> Command {
        let mut c = Command::new(program);
        c.env_clear().envs(self.vars.iter().map(|(k, v)| (k, v))).current_dir(self.dir.join("work"));
        c
    }

    fn tmux(&self, args: &[&str]) -> String {
        let out = self.cmd("tmux").args(args).output().unwrap();
        assert!(out.status.success(), "tmux {args:?}: {}", String::from_utf8_lossy(&out.stderr));
        String::from_utf8_lossy(&out.stdout).into_owned()
    }

    fn pane_of(&self, session: &str) -> String {
        self.tmux(&["display-message", "-p", "-t", &format!("={session}:"), "#{pane_id}"]).trim().into()
    }

    /// Run bud outside tmux.
    fn bud(&self, args: &[&str]) -> Output {
        self.cmd(BUD).args(args).stdin(Stdio::null()).output().unwrap()
    }

    /// Run bud as a command inside `session`'s agent pane.
    fn bud_in(&self, session: &str, args: &[&str]) -> Output {
        let pane = self.pane_of(session);
        self.cmd(BUD)
            .args(args)
            .env("TMUX", format!("{},{},0", self.socket, self.server_pid))
            .env("TMUX_PANE", pane)
            .stdin(Stdio::null())
            .output()
            .unwrap()
    }

    /// A session that bud did not launch, running the stub claude.
    fn root(&self, name: &str) {
        // Two arguments, so tmux runs the stub directly and not through a shell.
        let stub = self.dir.join("bin/claude").display().to_string();
        self.tmux(&["new-session", "-d", "-s", name, "-c", &self.dir.join("work").display().to_string(), &stub, "root"]);
        self.wait_for(|| self.state(name) == "done");
    }

    fn brief(&self, text: &str) -> String {
        let p = self.dir.join(format!("brief-{}.md", COUNTER.fetch_add(1, Ordering::SeqCst)));
        std::fs::write(&p, text).unwrap();
        p.display().to_string()
    }

    fn state(&self, session: &str) -> String {
        let out = self.cmd("tmux").args(["display-message", "-p", "-t", &format!("={session}:"), "#{@agent_state}"]).output().unwrap();
        String::from_utf8_lossy(&out.stdout).trim().into()
    }

    fn log(&self, session: &str, ext: &str) -> String {
        let pane = self.pane_of(session);
        std::fs::read_to_string(self.dir.join("log").join(format!("{}.{ext}", pane.trim_start_matches('%')))).unwrap_or_default()
    }

    fn argv(&self, session: &str) -> Vec<String> {
        let raw = self.log(session, "argv");
        raw.split('\0').filter(|s| !s.is_empty()).map(String::from).collect()
    }

    fn record(&self, name: &str) -> serde_json::Value {
        let p = self.dir.join("state/sessions").join(format!("{name}.json"));
        serde_json::from_str(&std::fs::read_to_string(p).unwrap()).unwrap()
    }

    fn wait_for(&self, f: impl Fn() -> bool) {
        let deadline = Instant::now() + Duration::from_secs(10);
        while !f() {
            assert!(Instant::now() < deadline, "timed out");
            std::thread::sleep(Duration::from_millis(100));
        }
    }

    /// A control-mode client attached to `session`, which counts as attached.
    fn attach_client(&mut self, session: &str) {
        let child = self
            .cmd("tmux")
            .args(["-C", "attach-session", "-t", &format!("={session}")])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .spawn()
            .unwrap();
        self.clients.push(child);
        self.wait_for(|| !self.tmux(&["list-clients", "-t", &format!("={session}")]).trim().is_empty());
    }

    fn git(&self, dir: &Path, args: &[&str]) {
        let out = self.cmd("git").arg("-C").arg(dir).args(args).output().unwrap();
        assert!(out.status.success(), "git {args:?}: {}", String::from_utf8_lossy(&out.stderr));
    }
}

impl Drop for Env {
    fn drop(&mut self) {
        for c in &mut self.clients {
            let _ = c.kill();
        }
        let _ = self.cmd("tmux").arg("kill-server").output();
        if std::env::var_os("BUD_KEEP").is_none() { let _ = std::fs::remove_dir_all(&self.dir); }
    }
}

fn ok(out: &Output) -> String {
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(out.status.success(), "failed:\n{stdout}\n{}", String::from_utf8_lossy(&out.stderr));
    stdout
}

fn err(out: &Output) -> String {
    assert!(!out.status.success(), "unexpected success:\n{}", String::from_utf8_lossy(&out.stdout));
    String::from_utf8_lossy(&out.stderr).into_owned()
}

#[test]
fn launch_adopts_the_parent_and_records_the_child() {
    let env = Env::new();
    env.root("main");
    let brief = env.brief("Review the thing.\nLine two with \"quotes\" and $HOME");
    let out = env.bud_in("main", &["launch", "kid", "--brief", &brief, "--harness", "claude", "--model", "m1", "--effort", "high"]);
    let stdout = ok(&out);
    assert!(stdout.contains("kid: started"), "{stdout}");

    let main = env.record("main");
    assert_eq!(main["adopted"], true);
    assert_eq!(main["harness"], "claude");
    assert_eq!(main["pane"], env.pane_of("main").as_str());

    let kid = env.record("kid");
    assert_eq!(kid["parent"], "main");
    let id = kid["session_id"].as_str().unwrap().to_string();
    let argv = env.argv("kid");
    let brief = "Review the thing.\nLine two with \"quotes\" and $HOME\n\nLaunched by `main` with bud; `bud send .. \"<text>\"` messages it.";
    assert_eq!(argv, ["--name", "kid", "--session-id", &id, "--model", "m1", "--effort", "high", "--", brief]);
    assert_eq!(kid["brief"], brief);

    let penv = env.log("kid", "env");
    assert!(penv.contains(&format!("BUD_STATE_DIR={}", env.dir.join("state").display())), "{penv}");
    assert!(!penv.contains("CLAUDECODE="), "nesting variable leaked");
    assert_eq!(env.tmux(&["show-options", "-v", "-t", "=kid:", "@bud_parent"]).trim(), "main");
    assert_eq!(env.tmux(&["show-options", "-wv", "-t", "=kid:", "remain-on-exit"]).trim(), "failed");
}

#[test]
fn tree_effective_parent_and_upward_send() {
    let env = Env::new();
    env.root("main");
    ok(&env.bud_in("main", &["launch", "a", "--brief", &env.brief("task a"), "--harness", "claude"]));
    // The child inherits its parent's harness.
    ok(&env.bud_in("a", &["launch", "b", "--brief", &env.brief("task b")]));
    assert_eq!(env.record("b")["harness"], "claude");

    let ls = ok(&env.bud_in("main", &["ls"]));
    let names: Vec<String> = ls
        .lines()
        .skip(1)
        .map(|l| {
            let t = l.trim_start();
            format!("{}{}", &l[..l.len() - t.len()], t.split("  ").next().unwrap())
        })
        .collect();
    assert_eq!(names, ["main *", "  a", "    b"], "{ls}");

    ok(&env.bud_in("b", &["send", "..", "hello from b"]));
    env.wait_for(|| env.log("a", "typed").contains("[from b] hello from b"));

    let out = ok(&env.bud(&["close", "a"]));
    assert!(out.contains("closed a") && out.contains("b now reports to main"), "{out}");
    ok(&env.bud_in("b", &["send", "..", "a is gone"]));
    env.wait_for(|| env.log("main", "typed").contains("[from b] a is gone"));

    let ls = ok(&env.bud_in("main", &["ls"]));
    assert!(ls.contains("\n  b "), "{ls}");
    let ls = ok(&env.bud_in("main", &["ls", "--closed"]));
    assert!(ls.contains("  a ") && ls.contains("closed") && ls.contains("    b"), "{ls}");
}

#[test]
fn close_tree_closes_descendants_first() {
    let env = Env::new();
    env.root("main");
    ok(&env.bud_in("main", &["launch", "a", "--brief", &env.brief("a"), "--harness", "claude"]));
    ok(&env.bud_in("a", &["launch", "b", "--brief", &env.brief("b")]));
    let out = ok(&env.bud_in("main", &["close", "a", "--tree"]));
    let (b, a) = (out.find("closed b").unwrap(), out.find("closed a").unwrap());
    assert!(b < a, "{out}");
    assert!(env.cmd("tmux").args(["has-session", "-t", "=b"]).stderr(Stdio::null()).status().map(|s| !s.success()).unwrap());
}

#[test]
fn adapters_build_each_harness_command() {
    let env = Env::new();
    env.root("main");
    let launch = |name: &str, harness: &str, extra: &[&str]| {
        let brief = env.brief("do it");
        let mut args = vec!["launch", name, "--brief", &brief, "--harness", harness, "--wait", "1"];
        args.extend(extra);
        env.bud_in("main", &args)
    };
    let footer = "\n\nLaunched by `main` with bud; `bud send .. \"<text>\"` messages it.";

    ok(&launch("cx", "codex", &["--skill", "review", "--model", "o9", "--effort", "low"]));
    assert_eq!(env.argv("cx"), ["--no-daemon", "-m", "o9", "-c", "model_reasoning_effort=\"low\"", "--", &format!("$review do it{footer}")]);

    ok(&launch("p", "pi", &["--skill", "review", "--effort", "high"]));
    let id = env.record("p")["session_id"].as_str().unwrap().to_string();
    assert_eq!(env.argv("p"), ["--name", "p", "--session-id", &id, "--thinking", "high", "--", &format!("/skill:review do it{footer}")]);

    ok(&launch("ag", "agy", &["--", "--mode", "plan"]));
    assert_eq!(env.argv("ag"), ["--mode".to_string(), "plan".into(), format!("--prompt-interactive=do it{footer}")]);

    ok(&launch("oc", "opencode", &[]));
    assert_eq!(env.argv("oc"), [".".to_string(), format!("--prompt=do it{footer}")]);

    // A /command is typed in after start.
    let out = ok(&launch("oc2", "opencode", &["--skill", "review"]));
    assert!(out.contains("sent the brief"), "{out}");
    assert_eq!(env.argv("oc2"), ["."]);
    env.wait_for(|| env.log("oc2", "typed").contains("/review do it"));

    assert!(err(&launch("oc3", "opencode", &["--effort", "high"])).contains("no effort setting"));
    assert!(err(&launch("zz", "nope", &[])).contains("unknown harness"));
}

#[test]
fn rejects_bad_launches() {
    let env = Env::new();
    let brief = env.brief("x");
    assert!(err(&env.bud(&["launch", "bad name", "--brief", &brief, "--harness", "claude"])).contains("invalid session name"));
    assert!(err(&env.bud(&["launch", "x", "--brief", &brief])).contains("--harness"));
    assert!(err(&env.bud(&["launch", "keepalive", "--brief", &brief, "--harness", "claude"])).contains("already exists"));
    let empty = env.brief("   ");
    assert!(err(&env.bud(&["launch", "e", "--brief", &empty, "--harness", "claude"])).contains("empty"));
}

#[test]
fn launch_outside_tmux_makes_a_root() {
    let env = Env::new();
    ok(&env.bud(&["launch", "solo", "--brief", &env.brief("alone"), "--harness", "claude"]));
    assert!(env.record("solo").get("parent").is_none());
    assert_eq!(env.argv("solo").last().unwrap(), "alone");
}

#[test]
fn send_refuses_dialogs_and_waits_for_typing() {
    let mut env = Env::new();
    ok(&env.bud(&["launch", "d", "--brief", &env.brief("DIALOG"), "--harness", "claude"]));
    assert!(err(&env.bud(&["send", "d", "x"])).contains("dialog"));

    ok(&env.bud(&["launch", "t", "--brief", &env.brief("plain"), "--harness", "claude"]));
    ok(&env.bud(&["send", "t", "first message"]));
    env.wait_for(|| env.log("t", "typed").contains("first message"));

    // An attached client that pressed a key within the idle time: paste, no Enter.
    env.vars.retain(|(k, _)| k != "BUD_IDLE_SECS");
    env.vars.push(("BUD_IDLE_SECS".into(), "1000".into()));
    env.attach_client("t");
    let out = ok(&env.bud(&["send", "t", "second message"]));
    assert!(out.contains("not submitted"), "{out}");
    std::thread::sleep(Duration::from_millis(500));
    assert!(!env.log("t", "typed").contains("second message"));
    env.tmux(&["send-keys", "-t", &env.pane_of("t"), "Enter"]);
    env.wait_for(|| env.log("t", "typed").contains("second message"));

    // --force submits at once.
    ok(&env.bud(&["send", "t", "third", "--force"]));
    env.wait_for(|| env.log("t", "typed").contains("third"));
}

#[test]
fn exits_keep_the_pane_and_resume_restarts() {
    let env = Env::new();
    let out = env.bud(&["launch", "x", "--brief", &env.brief("EXIT 3"), "--harness", "claude"]);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).contains("exited with status exited(3)"));
    let ls = ok(&env.bud(&["ls"]));
    assert!(ls.contains("exited(3)"), "{ls}");
    assert!(err(&env.bud(&["send", "x", "hi"])).contains("exited"));
    // Resume replaces the failed pane and continues by ID.
    let id = env.record("x")["session_id"].as_str().unwrap().to_string();
    ok(&env.bud(&["resume", "x"]));
    assert_eq!(env.argv("x"), ["--resume", &id]);
}

#[test]
fn resume_and_fresh() {
    let env = Env::new();
    let repo = env.dir.join("work/repo");
    std::fs::create_dir_all(&repo).unwrap();
    env.git(&repo, &["init", "-q"]);
    std::fs::write(repo.join("f"), "x").unwrap();
    env.git(&repo, &["add", "f"]);
    env.git(&repo, &["commit", "-qm", "init"]);
    let r = repo.display().to_string();

    ok(&env.bud(&["launch", "c", "--dir", &r, "--brief", &env.brief("work"), "--harness", "claude"]));
    let first = env.record("c")["session_id"].as_str().unwrap().to_string();
    assert!(err(&env.bud(&["resume", "c"])).contains("live"));
    let out = ok(&env.bud(&["close", "c"]));
    assert!(out.contains(&format!("{r}: clean")), "{out}");
    assert!(err(&env.bud(&["peek", "c"])).contains("closed at"));
    ok(&env.bud(&["resume", "c"]));
    assert_eq!(env.argv("c"), ["--resume", &first]);

    ok(&env.bud(&["close", "c"]));
    std::fs::write(repo.join("f"), "dirty").unwrap();
    assert!(err(&env.bud(&["resume", "c", "--fresh"])).contains("uncommitted"));
    ok(&env.bud(&["resume", "c", "--fresh", "--force"]));
    let second = env.record("c")["session_id"].as_str().unwrap().to_string();
    assert_ne!(first, second);
    assert_eq!(env.argv("c")[..4], ["--name", "c", "--session-id", second.as_str()]);

    // Without an ID, Codex continues the newest conversation in the directory.
    ok(&env.bud(&["launch", "k", "--dir", &r, "--share", "--brief", &env.brief("k"), "--harness", "codex"]));
    ok(&env.bud(&["close", "k"]));
    let out = ok(&env.bud(&["resume", "k"]));
    assert!(out.contains("warning") && out.contains("newest conversation"), "{out}");
    assert_eq!(env.argv("k"), ["resume", "--no-daemon", "--last"]);
}

#[test]
fn worktrees_and_shared_checkouts() {
    let env = Env::new();
    let repo = env.dir.join("work/proj");
    std::fs::create_dir_all(&repo).unwrap();
    env.git(&repo, &["init", "-q"]);
    std::fs::write(repo.join("f"), "x").unwrap();
    env.git(&repo, &["add", "f"]);
    env.git(&repo, &["commit", "-qm", "init"]);
    let r = repo.display().to_string();

    ok(&env.bud(&["launch", "one", "--dir", &r, "--brief", &env.brief("read"), "--harness", "claude"]));
    let msg = err(&env.bud(&["launch", "two", "--dir", &r, "--brief", &env.brief("read"), "--harness", "claude"]));
    assert!(msg.contains("--share"), "{msg}");
    ok(&env.bud(&["launch", "two", "--dir", &r, "--share", "--brief", &env.brief("read"), "--harness", "claude"]));

    let out = ok(&env.bud(&["launch", "feat", "--dir", &r, "--worktree", "feat", "--brief", &env.brief("edit"), "--harness", "claude"]));
    assert!(out.contains("worktree"), "{out}");
    let rec = env.record("feat");
    assert_eq!(rec["worktree"]["branch"], "feat");
    assert_ne!(rec["dir"], r.as_str());
    assert_eq!(env.log("feat", "cwd").trim(), rec["dir"].as_str().unwrap());
}

#[test]
fn close_reports_leftover_processes() {
    let env = Env::new();
    ok(&env.bud(&["launch", "s", "--brief", &env.brief("SPAWN"), "--harness", "claude"]));
    env.wait_for(|| !env.log("s", "spawned").is_empty());
    let pid = env.log("s", "spawned").trim().to_string();
    let out = ok(&env.bud(&["close", "s"]));
    assert!(out.contains(&format!("pid {pid}")), "{out}");
    let out = ok(&env.bud(&["strays"]));
    assert!(out.contains(&format!("pid {pid}")), "{out}");
    let _ = Command::new("kill").arg(&pid).status();
}

#[test]
fn stale_parent_pane_is_refused_then_repaired() {
    let env = Env::new();
    env.root("main");
    ok(&env.bud_in("main", &["launch", "kid", "--brief", &env.brief("k"), "--harness", "claude"]));
    // As after a server restart: the recorded pane is no longer in main.
    let p = env.dir.join("state/sessions/main.json");
    let mut rec = env.record("main");
    rec["pane"] = "%999".into();
    std::fs::write(&p, rec.to_string()).unwrap();
    assert!(err(&env.bud_in("kid", &["send", "..", "hi"])).contains("not in that session"));
    ok(&env.bud_in("main", &["ls"]));
    assert_eq!(env.record("main")["pane"], env.pane_of("main").as_str());
    ok(&env.bud_in("kid", &["send", "..", "hi"]));
}

#[test]
fn context_hook_reminds_parents_and_children() {
    let env = Env::new();
    env.root("main");
    assert!(ok(&env.bud_in("main", &["_context"])).is_empty());
    ok(&env.bud_in("main", &["launch", "kid", "--brief", &env.brief("k"), "--harness", "claude"]));
    let parent: serde_json::Value = serde_json::from_str(&ok(&env.bud_in("main", &["_context"]))).unwrap();
    let text = parent["hookSpecificOutput"]["additionalContext"].as_str().unwrap();
    assert!(text.contains("launched 1 child sessions with bud, 1 of them live"), "{text}");
    let child: serde_json::Value = serde_json::from_str(&ok(&env.bud_in("kid", &["_context"]))).unwrap();
    let text = child["hookSpecificOutput"]["additionalContext"].as_str().unwrap();
    assert!(text.contains("its parent is the session `main`"), "{text}");
}

#[test]
fn picker_rows_in_tree_order() {
    let mut env = Env::new();
    env.root("main");
    ok(&env.bud_in("main", &["launch", "a", "--brief", &env.brief("task\twith tab"), "--harness", "claude"]));
    ok(&env.bud_in("a", &["launch", "b", "--brief", &env.brief("task b")]));
    env.attach_client("a");
    let client = env.tmux(&["list-clients", "-F", "#{client_name}"]).trim().to_string();
    let out = ok(&env.bud(&["_pick", &client, "--rows"]));
    let mut lines = out.lines();
    let pos: usize = lines.next().unwrap().parse().unwrap();
    let rows: Vec<&str> = lines.collect();
    let names: Vec<&str> = rows.iter().map(|r| r.split('\t').next().unwrap()).collect();
    let order: Vec<&str> = names.iter().copied().filter(|n| *n != "keepalive").collect();
    assert_eq!(order, ["main", "a", "b"]);
    assert_eq!(names[pos - 1], "a");
    let a = rows.iter().find(|r| r.starts_with("a\t")).unwrap();
    assert_eq!(a.matches('\t').count(), 1, "tabs in the brief must not split the row: {a}");
    assert!(rows.iter().find(|r| r.starts_with("b\t")).unwrap().contains("\t    "));
}
