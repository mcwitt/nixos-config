//! bud: launch agent sessions in detached tmux sessions, as a tree, and
//! supervise them. The records under the state directory are the only source
//! of truth for the tree; tmux is the only source of truth for liveness.

mod adapter;
mod git;
mod procs;
mod record;
mod tmux;
mod tree;

use adapter::Adapter;
use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use record::{now, Record, Worktree};
use std::collections::HashSet;
use std::io::{Read, Write};
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use tree::World;

const MAX_BRIEF_BYTES: usize = 100_000; // Linux caps one argv string at 128 KiB

#[derive(Parser)]
#[command(name = "bud", about = "Launch agent sessions in tmux as a tree, and supervise them")]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Attach to the root session `bud`, or start it here in triage mode
    Up {
        #[arg(long, default_value = "claude")]
        harness: String,
        /// Continue the conversation of `bud` instead of starting a new one
        #[arg(long)]
        resume: bool,
    },
    /// Start a child of this session in a new tmux session
    Launch(LaunchArgs),
    /// Show this session's subtree
    Ls {
        /// Every tmux session, including sessions bud did not launch
        #[arg(long)]
        all: bool,
        /// Include closed sessions
        #[arg(long)]
        closed: bool,
        #[arg(long)]
        json: bool,
    },
    /// Pick a session from the tree in a popup and switch to it
    Pick { client: String },
    /// Show a session's state and the end of its screen
    Peek {
        /// A session name, or `..` for this session's parent
        target: String,
        #[arg(short = 'n', long, default_value_t = 20)]
        lines: usize,
    },
    /// Paste a message into a session and submit it
    Send {
        /// A session name, or `..` for this session's parent
        target: String,
        /// The message, or - to read it from standard input
        text: Option<String>,
        #[arg(long, conflicts_with = "text")]
        file: Option<PathBuf>,
        /// Send even on a dialog or prompt, and submit without waiting
        #[arg(long)]
        force: bool,
    },
    /// Close a session; report uncommitted changes and leftover processes
    Close {
        name: String,
        /// Also close its live descendants
        #[arg(long)]
        tree: bool,
    },
    /// Start a closed session again
    Resume {
        name: String,
        /// Start a new conversation from the same brief
        #[arg(long)]
        fresh: bool,
        /// With --fresh, allow uncommitted changes in the directory
        #[arg(long, requires = "fresh")]
        force: bool,
        #[arg(long)]
        wait: Option<f64>,
    },
    /// Print a session's record
    Show {
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// List processes whose tmux pane is gone
    Strays,
    #[command(name = "_run", hide = true)]
    Run { record: PathBuf },
    #[command(name = "_context", hide = true)]
    Context,
    #[command(name = "_pick", hide = true)]
    PickInner {
        client: String,
        #[arg(long)]
        rows: bool,
    },
}

#[derive(clap::Args)]
struct LaunchArgs {
    name: String,
    /// The brief: a file, or - for standard input
    #[arg(long)]
    brief: String,
    /// Working directory, or with --worktree the repository (default: here)
    #[arg(long)]
    dir: Option<PathBuf>,
    /// claude, codex, pi, opencode or agy (default: this session's harness)
    #[arg(long)]
    harness: Option<String>,
    /// Start the brief with this skill, in the child harness's syntax
    #[arg(long)]
    skill: Option<String>,
    #[arg(long)]
    model: Option<String>,
    #[arg(long)]
    effort: Option<String>,
    /// Run in a worktree of --dir for BRANCH (or pr:N), made if needed
    #[arg(long, value_name = "BRANCH")]
    worktree: Option<String>,
    /// Base for a new --worktree branch (default: the default branch)
    #[arg(long, requires = "worktree")]
    base: Option<String>,
    /// Allow a checkout that another live session uses
    #[arg(long)]
    share: bool,
    /// Seconds to wait for the agent to start
    #[arg(long)]
    wait: Option<f64>,
    /// Extra arguments for the harness
    #[arg(last = true)]
    harness_args: Vec<String>,
}

fn main() {
    let cli = Cli::parse();
    match dispatch(cli.cmd) {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            eprintln!("bud: {e:#}");
            std::process::exit(1);
        }
    }
}

fn dispatch(cmd: Cmd) -> Result<i32> {
    match cmd {
        Cmd::Up { harness, resume } => up(&harness, resume),
        Cmd::Launch(a) => launch(a),
        Cmd::Ls { all, closed, json } => ls(all, closed, json),
        Cmd::Pick { client } => pick(&client),
        Cmd::Peek { target, lines } => peek(&target, lines),
        Cmd::Send { target, text, file, force } => send(&target, text, file, force),
        Cmd::Close { name, tree } => close(&name, tree),
        Cmd::Resume { name, fresh, force, wait } => resume(&name, fresh, force, wait),
        Cmd::Show { name, json } => show(&name, json),
        Cmd::Strays => strays(),
        Cmd::Run { record } => run(&record),
        Cmd::Context => context(),
        Cmd::PickInner { client, rows } => pick_inner(&client, rows),
    }
}

// ------------------------------------------------------------- helpers

/// The session this command runs in. Its recorded pane is updated when it
/// is stale, as after a restart of the tmux server; a live agent pane is
/// never replaced, so a command run from a shell pane does not take over.
fn caller(world: &mut World) -> Option<String> {
    let pane = tmux::current_pane()?;
    let name = world.snap.session_of_pane(&pane)?.to_string();
    let stale = world
        .recs
        .get(&name)
        .map(|r| r.pane.as_deref().is_none_or(|p| world.snap.pane(&name, p).is_none()));
    if stale == Some(true) {
        let rec = world.recs.get_mut(&name).unwrap();
        rec.pane = Some(pane);
        let _ = record::save(rec);
    }
    Some(name)
}

fn resolve(target: &str, world: &World, me: Option<&str>) -> Result<String> {
    if target != ".." {
        return Ok(target.to_string());
    }
    let Some(me) = me else { bail!("`..` means this session's parent, but this command does not run in tmux") };
    match world.effective_parent(me) {
        Some(p) => Ok(p),
        None => bail!("{me} has no live parent"),
    }
}

/// The agent pane of a live session, or an error that says why there is none.
fn require_pane(world: &World, name: &str) -> Result<String> {
    if let Some(p) = world.agent_pane(name) {
        return Ok(p.id.clone());
    }
    if world.snap.sessions.contains_key(name) {
        let rec = &world.recs[name];
        bail!(
            "the recorded agent pane of {name} ({}) is not in that session, so bud does not send input there. \
             Run any bud command in {name} to record its pane again, or close and resume it.",
            rec.pane.as_deref().unwrap_or("none")
        );
    }
    match world.recs.get(name).and_then(|r| r.closed_at) {
        Some(t) => bail!("no session {name}: it was closed at {}", clock(t)),
        None if world.recs.contains_key(name) => {
            bail!("no session {name}: its agent exited or the session was killed; `bud resume {name}` starts it again")
        }
        None => bail!("no session {name}"),
    }
}

fn clock(t: u64) -> String {
    let out = Command::new("date").args(["-d", &format!("@{t}"), "+%Y-%m-%d %H:%M"]).output();
    match out {
        Ok(o) if o.status.success() => String::from_utf8_lossy(&o.stdout).trim().to_string(),
        _ => t.to_string(),
    }
}

fn age(secs: u64) -> String {
    for (unit, size) in [("d", 86400), ("h", 3600), ("m", 60)] {
        if secs >= size {
            return format!("{}{unit}", secs / size);
        }
    }
    format!("{secs}s")
}

fn tilde(p: &str) -> String {
    let home = record::home().display().to_string();
    match p.strip_prefix(&home) {
        Some(rest) if !home.is_empty() => format!("~{rest}"),
        _ => p.to_string(),
    }
}

fn read_text(arg: &str) -> Result<String> {
    let text = if arg == "-" {
        let mut s = String::new();
        std::io::stdin().read_to_string(&mut s)?;
        s
    } else {
        std::fs::read_to_string(arg).with_context(|| format!("reading {arg}"))?
    };
    Ok(text.trim().to_string())
}

fn valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_alphanumeric())
        && name.len() <= 48
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

fn on_path(program: &str) -> bool {
    std::env::var_os("PATH")
        .is_some_and(|p| std::env::split_paths(&p).any(|d| d.join(program).is_file()))
}

fn env_secs(var: &str, default: f64) -> f64 {
    std::env::var(var).ok().and_then(|v| v.parse().ok()).unwrap_or(default)
}

fn first_line(s: &str, max: usize) -> String {
    s.lines().next().unwrap_or("").chars().take(max).collect()
}

fn attach(name: &str) -> Result<i32> {
    let verb = if std::env::var_os("TMUX").is_some() { "switch-client" } else { "attach-session" };
    let err = Command::new("tmux").args([verb, "-t", &format!("={name}")]).exec();
    bail!("tmux {verb}: {err}")
}

// ------------------------------------------------------------- start

/// Start the tmux session for `rec`, running `rec.command`, and report.
fn start(mut rec: Record, ad: &Adapter, wait: Option<f64>, fresh: bool) -> Result<i32> {
    let name = rec.name.clone();
    if tmux::has_session(&name) {
        bail!("session {name} already exists; attach with: tmux attach -t {name}");
    }
    if !Path::new(&rec.dir).is_dir() {
        bail!("no such directory: {}", rec.dir);
    }
    if !on_path(ad.program) {
        bail!("{} is not on PATH", ad.program);
    }
    rec.command = if fresh { ad.launch(&rec)? } else { ad.resume(&rec) };
    rec.closed_at = None;
    rec.pane = None;
    record::save(&rec)?;

    let exe = std::env::current_exe()?.display().to_string();
    let state = std::fs::canonicalize(record::state_dir())?;
    let path = record::path(&name).display().to_string();
    let env = format!("BUD_STATE_DIR={}", state.display());
    // Not `=name`: inside tmux, an exact-match target chained after
    // new-session finds no session. The name was just created, so a plain
    // target matches it exactly.
    let window = format!("{name}:");
    let mut args = vec![
        "new-session", "-d", "-P", "-F", "#{pane_id}", "-s", &name, "-c", &rec.dir, "-e", &env,
        &exe, "_run", &path,
        // In the same command queue, before tmux handles an early exit.
        ";", "set-option", "-w", "-t", &window, "remain-on-exit", "failed",
    ]; // fmt: skip
    let parent = rec.parent.clone().unwrap_or_default();
    if !parent.is_empty() {
        args.extend([";", "set-option", "-t", &window, "@bud_parent", &parent]);
    }
    let pane = tmux::run(&args)?.trim().to_string();
    rec.pane = Some(pane.clone());
    record::save(&rec)?;

    let wait = wait.unwrap_or(if ad.reports_state { 15.0 } else { 8.0 });
    let deadline = Instant::now() + Duration::from_secs_f64(wait);
    while Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(250));
        let Some(info) = tmux::display(&pane, &format!("#{{pane_dead}}{}#{{@agent_state}}", tmux::SEP)) else { break };
        let (dead, state) = info.split_once(tmux::SEP).unwrap_or((&info, ""));
        if dead == "1" || (ad.reports_state && !state.is_empty()) {
            break;
        }
    }

    let world = World::read();
    if !world.snap.sessions.contains_key(&name) {
        println!("{name}: exited during startup");
        return Ok(1);
    }
    let tail = tmux::capture(&pane, 8);
    let show_tail = || tail.iter().for_each(|l| println!("  | {l}"));
    if world.agent_pane(&name).is_some_and(|p| p.dead) {
        println!("{name}: {} exited with status {}; the pane stays open", ad.program, world.state(&name));
        show_tail();
        return Ok(1);
    }
    println!("{name}: started in {}, state {}", rec.dir, world.state(&name));
    show_tail();
    let brief = rec.brief.clone().unwrap_or_default();
    let typed = fresh && ad.brief_after_start(&brief);
    if tmux::dialog_open(&pane) {
        println!(
            "{name} shows a dialog (above), such as a folder-trust or hook-review check. Ask the user to attach and answer it."
        );
        if typed {
            println!("The brief is not sent yet: after the dialog, run `bud send {name} --file <brief>`.");
        } else {
            println!("The brief waits behind the dialog.");
        }
    } else if typed {
        paste(&pane, &brief, true)?;
        println!("sent the brief to {name}");
    }
    if let Some(id) = &rec.session_id {
        println!("session id {id}");
    }
    println!("attach: tmux attach -t {name}");
    Ok(0)
}

fn paste(pane: &str, text: &str, submit: bool) -> Result<()> {
    // A bracketed paste keeps newlines from submitting early. Enter goes
    // separately after a pause: Codex reads an Enter inside a burst of
    // input as a newline.
    let buf = format!("bud-{}", std::process::id());
    tmux::run_with_input(&["load-buffer", "-b", &buf, "-"], text)?;
    tmux::run(&["paste-buffer", "-p", "-d", "-b", &buf, "-t", pane])?;
    if submit {
        std::thread::sleep(Duration::from_secs_f64(env_secs("BUD_SEND_DELAY", 0.5)));
        tmux::run(&["send-keys", "-t", pane, "Enter"])?;
    }
    Ok(())
}

// ------------------------------------------------------------- commands

fn launch(a: LaunchArgs) -> Result<i32> {
    if !valid_name(&a.name) {
        bail!("invalid session name {:?}: use letters, digits, '-' and '_' (at most 48)", a.name);
    }
    let text = read_text(&a.brief)?;
    let mut world = World::read();
    let me = caller(&mut world);
    if tmux::has_session(&a.name) {
        bail!("session {} already exists; attach with: tmux attach -t {}", a.name, a.name);
    }
    let parent_harness = me.as_ref().and_then(|m| world.recs.get(m)).and_then(|r| r.harness.clone());
    let Some(harness) = a.harness.clone().or(parent_harness) else {
        bail!("this session has no record yet, so bud does not know its harness: pass --harness")
    };
    let ad = adapter::get(&harness)?;
    let cwd = std::env::current_dir()?;
    let base_dir = std::fs::canonicalize(a.dir.clone().unwrap_or(cwd))?.display().to_string();
    let dir = match &a.worktree {
        Some(branch) => git::worktree(&base_dir, branch, a.base.as_deref())?,
        None => base_dir.clone(),
    };
    if let (Some(top), false) = (git::toplevel(&dir), a.share) {
        let users: Vec<&String> = world
            .snap
            .sessions
            .iter()
            .filter(|(n, s)| n.as_str() != a.name && git::toplevel(&s.path).as_deref() == Some(&top))
            .map(|(n, _)| n)
            .collect();
        if !users.is_empty() {
            let names: Vec<&str> = users.iter().map(|s| s.as_str()).collect();
            bail!(
                "{top} is already the working directory of session(s) {}. Use a separate worktree \
                 (--worktree) for a session that edits or checks out, or pass --share if both only read.",
                names.join(", ")
            );
        }
    }

    // A session that bud did not launch becomes a parent: record it first.
    if let Some(m) = &me {
        if !world.recs.contains_key(m) {
            let s = &world.snap.sessions[m];
            record::save(&Record {
                name: m.clone(),
                harness: Some(harness.clone()),
                pane: tmux::current_pane(),
                dir: s.path.clone(),
                launched_at: s.created,
                adopted: true,
                ..Default::default()
            })?;
        }
    }

    let mut brief = match &a.skill {
        Some(s) if text.is_empty() => ad.skill(s),
        Some(s) => format!("{} {text}", ad.skill(s)),
        None => text,
    };
    if brief.is_empty() {
        bail!("the brief is empty");
    }
    if let Some(m) = &me {
        brief.push_str(&format!("\n\nLaunched by `{m}` with bud; `bud send .. \"<text>\"` messages it."));
    }
    if brief.len() > MAX_BRIEF_BYTES {
        bail!("the brief is over {MAX_BRIEF_BYTES} bytes; put the detail in a file and point the brief at it");
    }
    let rec = Record {
        name: a.name.clone(),
        parent: me,
        harness: Some(harness),
        dir,
        worktree: a.worktree.map(|branch| Worktree { repo: base_dir, branch }),
        brief: Some(brief),
        model: a.model,
        effort: a.effort,
        args: a.harness_args,
        session_id: ad.id_at_launch.then(record::uuid),
        launched_at: now(),
        ..Default::default()
    };
    start(rec, ad, a.wait, true)
}

fn up(harness: &str, resume_root: bool) -> Result<i32> {
    if tmux::has_session("bud") {
        return attach("bud");
    }
    let status = if resume_root {
        let Some(rec) = record::load("bud") else { bail!("there is no record of `bud`; run `bud up` without --resume") };
        let ad = adapter::get(rec.harness.as_deref().unwrap_or(harness))?;
        start(rec, ad, None, false)?
    } else {
        let ad = adapter::get(harness)?;
        let rec = Record {
            name: "bud".into(),
            harness: Some(ad.name.into()),
            dir: std::env::current_dir()?.display().to_string(),
            brief: Some(ad.skill("bud")),
            session_id: ad.id_at_launch.then(record::uuid),
            launched_at: now(),
            ..Default::default()
        };
        start(rec, ad, None, true)?
    };
    if status != 0 {
        return Ok(status);
    }
    attach("bud")
}

fn ls(all: bool, closed: bool, json: bool) -> Result<i32> {
    let mut world = World::read();
    let me = caller(&mut world);
    let mut nodes: HashSet<String> = world
        .recs
        .keys()
        .filter(|n| closed || world.snap.sessions.contains_key(n.as_str()))
        .cloned()
        .collect();
    if all {
        nodes.extend(world.snap.sessions.keys().cloned());
    }
    let root = me.as_deref().filter(|m| !all && nodes.contains(*m));
    let rows = world.layout(&nodes, root);
    let t = now();
    if json {
        let out: Vec<serde_json::Value> = rows
            .iter()
            .map(|(n, depth)| {
                let rec = world.recs.get(n);
                let s = world.snap.sessions.get(n);
                serde_json::json!({
                    "name": n,
                    "depth": depth,
                    "self": me.as_deref() == Some(n.as_str()),
                    "parent": world.effective_parent(n),
                    "harness": rec.and_then(|r| r.harness.clone()),
                    "state": world.state(n),
                    "live": world.is_live(n),
                    "attached": s.is_some_and(|s| s.attached),
                    "dir": rec.map(|r| r.dir.clone()).or(s.map(|s| s.path.clone())),
                    "session_id": rec.and_then(|r| r.session_id.clone()),
                    "brief": rec.and_then(|r| r.brief.as_deref()).map(|b| first_line(b, 200)),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&out)?);
        return Ok(0);
    }
    if rows.is_empty() {
        println!("no bud sessions (`bud ls --all` shows every tmux session)");
        return Ok(0);
    }
    let mut table = vec![["NAME", "HARNESS", "STATE", "ATT", "AGE", "IDLE", "DIR", "BRIEF"].map(String::from)];
    for (n, depth) in &rows {
        let rec = world.recs.get(n);
        let s = world.snap.sessions.get(n);
        let started = s.map(|s| s.created).or(rec.map(|r| r.launched_at)).unwrap_or(t);
        let brief = match rec {
            Some(r) if r.adopted => "(not launched by bud)".to_string(),
            Some(r) => first_line(r.brief.as_deref().unwrap_or(""), 70),
            None => "(not launched by bud)".to_string(),
        };
        let mark = if me.as_deref() == Some(n.as_str()) { " *" } else { "" };
        table.push([
            format!("{}{n}{mark}", "  ".repeat(*depth)),
            rec.and_then(|r| r.harness.clone()).unwrap_or("-".into()),
            world.state(n),
            if s.is_some_and(|s| s.attached) { "yes".into() } else { String::new() },
            age(t.saturating_sub(started)),
            s.map(|s| age(t.saturating_sub(s.activity))).unwrap_or("-".into()),
            tilde(&rec.map(|r| r.dir.clone()).or(s.map(|s| s.path.clone())).unwrap_or_default()),
            brief,
        ]);
    }
    let widths: Vec<usize> = (0..7).map(|i| table.iter().map(|r| r[i].chars().count()).max().unwrap()).collect();
    for row in &table {
        let mut line = String::new();
        for (i, w) in widths.iter().enumerate() {
            line.push_str(&format!("{:<w$}  ", row[i], w = w));
        }
        println!("{line}{}", row[7]);
    }
    Ok(0)
}

fn peek(target: &str, lines: usize) -> Result<i32> {
    let mut world = World::read();
    let me = caller(&mut world);
    let name = resolve(target, &world, me.as_deref())?;
    let pane = require_pane(&world, &name)?;
    println!("{name}: state {}", world.state(&name));
    for l in tmux::capture(&pane, lines) {
        println!("{l}");
    }
    Ok(0)
}

/// Wait until no client attached to `name` has had a key press for the
/// idle time. False if the clients are still active at the deadline.
fn wait_idle(name: &str) -> bool {
    let idle = env_secs("BUD_IDLE_SECS", 15.0) as u64;
    let deadline = Instant::now() + Duration::from_secs_f64(env_secs("BUD_IDLE_MAX", 120.0));
    let mut said = false;
    loop {
        let out = tmux::run(&["list-clients", "-t", &format!("={name}"), "-F", "#{client_activity}"]).unwrap_or_default();
        let t = now();
        let active = out.lines().any(|a| a.trim().parse::<u64>().is_ok_and(|a| t.saturating_sub(a) < idle));
        if !active {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        if !said {
            println!("someone is typing in {name}; waiting until its clients are idle for {idle}s");
            said = true;
        }
        std::thread::sleep(Duration::from_millis(500));
    }
}

fn send(target: &str, text: Option<String>, file: Option<PathBuf>, force: bool) -> Result<i32> {
    let text = match (text, file) {
        (_, Some(f)) => read_text(&f.display().to_string())?,
        (Some(t), None) if t == "-" => read_text("-")?,
        (Some(t), None) => t.trim().to_string(),
        (None, None) => bail!("give the message, --file F, or - for standard input"),
    };
    if text.is_empty() {
        bail!("nothing to send");
    }
    let mut world = World::read();
    let me = caller(&mut world);
    let name = resolve(target, &world, me.as_deref())?;
    let pane = require_pane(&world, &name)?;
    if !world.is_live(&name) {
        bail!("{name}: its agent exited ({}); the pane shows why", world.state(&name));
    }
    let state = world.state(&name);
    if (state == "input" || tmux::dialog_open(&pane)) && !force {
        bail!(
            "{name} waits on a dialog, permission prompt or question, which typed text could answer. \
             Look with `bud peek {name}`, then answer it there, or pass --force."
        );
    }
    if state == "working" {
        println!("{name} is mid-turn: the agent takes the message when it can");
    }
    let msg = match &me {
        Some(m) if *m != name => format!("[from {m}] {text}"),
        _ => text,
    };
    let submit = force || wait_idle(&name);
    paste(&pane, &msg, submit)?;
    if submit {
        println!("sent {} characters to {name}", msg.chars().count());
        return Ok(0);
    }
    let from = me.as_deref().unwrap_or("bud");
    let clients = tmux::run(&["list-clients", "-t", &format!("={name}"), "-F", "#{client_name}"]).unwrap_or_default();
    for c in clients.lines() {
        let note = format!("bud: message from {from} pasted into {name}, not submitted");
        let _ = tmux::run(&["display-message", "-d", "3000", "-c", c, &note]);
    }
    println!("{name}: someone kept typing there, so the message is pasted but not submitted; the user must press Enter");
    Ok(0)
}

fn close(name: &str, subtree: bool) -> Result<i32> {
    let mut world = World::read();
    caller(&mut world);
    if !world.snap.sessions.contains_key(name) {
        require_pane(&world, name)?;
    }
    let moving = if subtree { vec![] } else { world.children(name) };
    let mut targets = if subtree { world.descendants(name) } else { vec![] };
    targets.push(name.to_string());
    let socket = tmux::run(&["display-message", "-p", "#{socket_path}"]).unwrap_or_default();
    for t in &targets {
        close_one(&world, t, socket.trim())?;
    }
    let world = World::read();
    for c in moving {
        match world.effective_parent(&c) {
            Some(p) => println!("{c} now reports to {p}"),
            None => println!("{c} is now a root"),
        }
    }
    Ok(0)
}

fn close_one(world: &World, name: &str, socket: &str) -> Result<()> {
    let dir = world.snap.sessions.get(name).map(|s| s.path.clone());
    let panes: HashSet<String> = world.snap.session_panes(name).iter().map(|p| p.id.clone()).collect();
    let pids: Vec<u32> = world.snap.session_panes(name).iter().map(|p| p.pid).collect();
    let procs = if procs::SUPPORTED { procs::descendants(&pids) } else { HashSet::new() };
    tmux::run(&["kill-session", "-t", &format!("={name}")])?;
    // Return only once the agent has exited, so that a resume right after
    // never runs alongside the old process on the same conversation.
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut alive: Vec<u32> = procs.iter().copied().filter(|p| procs::alive(*p)).collect();
    while !alive.is_empty() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(100));
        alive.retain(|p| procs::alive(*p));
    }
    println!("closed {name}");
    if !alive.is_empty() {
        println!("warning: {} process(es) of {name} still running 10s after the hangup", alive.len());
    }
    if let Some(mut rec) = world.recs.get(name).cloned() {
        rec.closed_at = Some(now());
        record::save(&rec)?;
    }
    if let Some(d) = dir.filter(|d| !d.is_empty()) {
        match git::status(&d) {
            Some(s) if s.is_empty() => println!("{d}: clean"),
            Some(s) => println!("{d}: uncommitted changes:\n{s}"),
            None => {}
        }
    }
    if !procs::SUPPORTED {
        println!("leftover processes: not checked on this system");
        return Ok(());
    }
    std::thread::sleep(Duration::from_secs_f64(env_secs("BUD_CLOSE_GRACE", 1.0)));
    let live: HashSet<String> = tmux::run(&["list-panes", "-a", "-F", "#{pane_id}"])
        .unwrap_or_default()
        .split_whitespace()
        .map(String::from)
        .collect();
    let strays = procs::strays(socket, &live, Some(&panes));
    if !strays.is_empty() {
        println!("still running from {name} (not killed; stop them if they are not wanted):");
        procs::print(&strays);
    }
    Ok(())
}

fn resume(name: &str, fresh: bool, force: bool, wait: Option<f64>) -> Result<i32> {
    let mut world = World::read();
    caller(&mut world);
    let Some(mut rec) = world.recs.get(name).cloned() else { bail!("no record of {name}") };
    if rec.adopted {
        bail!("{name} was not launched by bud, so there is no brief or command to start it from");
    }
    if world.is_live(name) {
        bail!("{name} is live; attach with: tmux attach -t {name}");
    }
    if world.snap.sessions.contains_key(name) {
        // A failed pane that tmux kept open, or a different session that took the name.
        if world.recs[name].pane.as_deref().is_some_and(|p| world.snap.pane(name, p).is_some()) {
            tmux::run(&["kill-session", "-t", &format!("={name}")])?;
        } else {
            bail!("a session named {name} exists but is not the one recorded; close or rename it first");
        }
    }
    let ad = adapter::get(rec.harness.as_deref().unwrap_or("claude"))?;
    if fresh {
        if let Some(s) = git::status(&rec.dir).filter(|s| !s.is_empty()) {
            if !force {
                bail!("{} has uncommitted changes; pass --force to start a new conversation anyway:\n{s}", rec.dir);
            }
        }
        rec.session_id = ad.id_at_launch.then(record::uuid);
    } else if ad.resumes_by_directory(&rec) {
        let shared: Vec<&String> = world
            .recs
            .values()
            .filter(|r| r.name != name && r.dir == rec.dir && world.is_live(&r.name))
            .map(|r| &r.name)
            .collect();
        if !shared.is_empty() {
            let names: Vec<&str> = shared.iter().map(|s| s.as_str()).collect();
            println!(
                "warning: {name} has no conversation ID, so {} continues the newest conversation in {}, \
                 which {} also use; check that it is the right one",
                ad.program,
                rec.dir,
                names.join(", ")
            );
        }
    }
    start(rec, ad, wait, fresh)
}

fn show(name: &str, json: bool) -> Result<i32> {
    let Some(rec) = record::load(name) else { bail!("no record of {name}") };
    if json {
        println!("{}", serde_json::to_string_pretty(&rec)?);
        return Ok(0);
    }
    let opt = |o: &Option<String>| o.clone().unwrap_or("-".into());
    println!("name        {}", rec.name);
    println!("parent      {}", opt(&rec.parent));
    println!("harness     {}", opt(&rec.harness));
    println!("dir         {}", rec.dir);
    if let Some(w) = &rec.worktree {
        println!("worktree    {} of {}", w.branch, w.repo);
    }
    println!("session id  {}", opt(&rec.session_id));
    println!("launched    {}", clock(rec.launched_at));
    if let Some(t) = rec.closed_at {
        println!("closed      {}", clock(t));
    }
    if !rec.command.is_empty() {
        println!("command     {}", rec.command[..rec.command.len().min(8)].join(" "));
    }
    if let Some(b) = &rec.brief {
        println!("brief:\n{b}");
    }
    Ok(0)
}

fn strays() -> Result<i32> {
    if !procs::SUPPORTED {
        println!("leftover processes: not checked on this system");
        return Ok(0);
    }
    let socket = tmux::run(&["display-message", "-p", "#{socket_path}"]).unwrap_or_default();
    let live: HashSet<String> = tmux::run(&["list-panes", "-a", "-F", "#{pane_id}"])
        .unwrap_or_default()
        .split_whitespace()
        .map(String::from)
        .collect();
    let strays = procs::strays(socket.trim(), &live, None);
    if strays.is_empty() {
        println!("no processes left over from closed tmux panes");
    } else {
        println!("processes left over from closed tmux panes:");
        procs::print(&strays);
    }
    Ok(0)
}

/// The pane process: exec the record's command.
fn run(path: &Path) -> Result<i32> {
    let rec: Record = serde_json::from_str(&std::fs::read_to_string(path)?)?;
    let Some((program, args)) = rec.command.split_first() else { bail!("{} has no command", path.display()) };
    let ad = adapter::get(rec.harness.as_deref().unwrap_or("claude"))?;
    let mut cmd = Command::new(program);
    cmd.args(args);
    if let Some(state) = path.parent().and_then(Path::parent) {
        cmd.env("BUD_STATE_DIR", state);
    }
    for v in ad.nesting_env {
        cmd.env_remove(v);
    }
    let err = cmd.exec();
    eprintln!("bud: {program}: {err}");
    Ok(127)
}

/// The SessionStart hook: remind a session of its place in the tree.
fn context() -> Result<i32> {
    let mut sink = Vec::new();
    let _ = std::io::stdin().read_to_end(&mut sink);
    let mut world = World::read();
    let Some(me) = caller(&mut world) else { return Ok(0) };
    let launched = world.recs.values().filter(|r| r.parent.as_deref() == Some(me.as_str())).count();
    let parent = world.recs.get(&me).and_then(|r| r.parent.clone());
    if launched == 0 && parent.is_none() {
        return Ok(0);
    }
    let mut parts = Vec::new();
    match (&parent, world.effective_parent(&me)) {
        (_, Some(p)) => parts.push(format!(
            "This session ({me}) was launched with bud; its parent is the session `{p}`, and `bud send .. \"<text>\"` messages it."
        )),
        (Some(p), None) => parts.push(format!("This session ({me}) was launched with bud by `{p}`, which is gone.")),
        _ => {}
    }
    if launched > 0 {
        let live = world.children(&me).len();
        parts.push(format!(
            "This session launched {launched} child sessions with bud, {live} of them live (`bud ls`). \
             If the user turned on triage mode with the bud skill, it is still on."
        ));
    }
    parts.push("Read the bud skill before you act on these sessions.".into());
    let out = serde_json::json!({
        "hookSpecificOutput": {"hookEventName": "SessionStart", "additionalContext": parts.join(" ")}
    });
    println!("{out}");
    Ok(0)
}

fn pick(client: &str) -> Result<i32> {
    let exe = std::env::current_exe()?.display().to_string();
    let path = format!("PATH={}", std::env::var("PATH").unwrap_or_default());
    let mut args = vec!["display-popup", "-c", client, "-E", "-w", "90%", "-h", "80%", "-e", &path];
    let state = format!("BUD_STATE_DIR={}", record::state_dir().display());
    args.extend(["-e", &state, &exe, "_pick", client]);
    tmux::run(&args)?;
    Ok(0)
}

fn glyph(state: &str) -> &'static str {
    match state {
        "input" => "\x1b[1;31m!\x1b[0m",
        "done" => "\x1b[32m✓\x1b[0m",
        "working" => "●",
        "background" => "\x1b[34m◐\x1b[0m",
        s if s.starts_with("exited") => "\x1b[31m✗\x1b[0m",
        _ => " ",
    }
}

fn pick_inner(client: &str, rows_only: bool) -> Result<i32> {
    let world = World::read();
    let nodes: HashSet<String> = world.snap.sessions.keys().cloned().collect();
    let current = tmux::run(&["display-message", "-p", "-c", client, "#{session_name}"])
        .unwrap_or_default()
        .trim()
        .to_string();
    let mut rows = Vec::new();
    let mut pos = 1;
    for (i, (n, depth)) in world.layout(&nodes, None).iter().enumerate() {
        if *n == current {
            pos = i + 1;
        }
        let rec = world.recs.get(n);
        let indent = "  ".repeat((*depth).min(6)) + if *depth > 6 { "… " } else { "" };
        let brief: String = rec
            .and_then(|r| r.brief.as_deref())
            .map(|b| first_line(b, 100))
            .unwrap_or_default()
            .chars()
            .map(|c| if c.is_control() { ' ' } else { c })
            .collect();
        let harness = rec.and_then(|r| r.harness.clone()).unwrap_or("-".into());
        rows.push(format!("{n}\t{indent}{} {n}  \x1b[2m{harness}  {brief}\x1b[0m", glyph(&world.state(n))));
    }
    if rows_only {
        println!("{pos}");
        rows.iter().for_each(|r| println!("{r}"));
        return Ok(0);
    }
    let mut child = Command::new("fzf")
        .args(["--delimiter", "\t", "--with-nth", "2..", "--accept-nth", "1", "--ansi", "--no-sort", "--exact"])
        .args(["--layout", "reverse", "--sync", "--bind", &format!("start:pos({pos})")])
        .env_remove("FZF_DEFAULT_OPTS")
        .env_remove("FZF_DEFAULT_OPTS_FILE")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .context("running fzf")?;
    child.stdin.take().unwrap().write_all((rows.join("\n") + "\n").as_bytes())?;
    let out = child.wait_with_output()?;
    let target = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if !out.status.success() || target.is_empty() {
        return Ok(0); // Esc, or nothing chosen
    }
    if !tmux::has_session(&target) {
        eprintln!("{target} is gone");
        std::thread::sleep(Duration::from_secs(1));
        return Ok(1);
    }
    tmux::run(&["switch-client", "-c", client, "-t", &format!("={target}")])?;
    Ok(0)
}
