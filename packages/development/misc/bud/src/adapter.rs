//! What bud knows about each agent harness. Nothing else in bud, and nothing
//! in the skill, names a harness flag. Flags checked against `--help` of
//! Claude Code 2.1.316, Codex 0.166.0, pi 1.4.0, OpenCode 1.15.10 and
//! Antigravity 1.4.12.

use crate::record::Record;
use anyhow::{bail, Result};

pub struct Adapter {
    pub name: &'static str,
    pub program: &'static str,
    /// The harness accepts a conversation ID at launch.
    pub id_at_launch: bool,
    /// The harness publishes @agent_state through its hooks.
    pub reports_state: bool,
    /// Variables that make the harness refuse to start or misbehave when it
    /// inherits them from another agent's process.
    pub nesting_env: &'static [&'static str],
}

pub const ADAPTERS: &[Adapter] = &[
    Adapter {
        name: "claude",
        program: "claude",
        id_at_launch: true,
        reports_state: true,
        nesting_env: &["CLAUDECODE", "CLAUDE_CODE_ENTRYPOINT"],
    },
    Adapter {
        name: "codex",
        program: "codex",
        id_at_launch: false,
        reports_state: true,
        nesting_env: &[],
    },
    Adapter {
        name: "pi",
        program: "pi",
        id_at_launch: true,
        reports_state: true,
        nesting_env: &[],
    },
    Adapter {
        name: "opencode",
        program: "opencode",
        id_at_launch: false,
        reports_state: true,
        nesting_env: &[],
    },
    Adapter {
        name: "agy",
        program: "agy",
        id_at_launch: false,
        reports_state: false,
        nesting_env: &[],
    },
];

pub fn get(name: &str) -> Result<&'static Adapter> {
    let name = if name == "antigravity" { "agy" } else { name };
    match ADAPTERS.iter().find(|a| a.name == name) {
        Some(a) => Ok(a),
        None => {
            let known: Vec<&str> = ADAPTERS.iter().map(|a| a.name).collect();
            bail!("unknown harness {name:?}; known: {}", known.join(", "))
        }
    }
}

impl Adapter {
    /// How a message invokes skill `s`.
    pub fn skill(&self, s: &str) -> String {
        match self.name {
            "codex" => format!("${s}"),
            "pi" => format!("/skill:{s}"),
            _ => format!("/{s}"),
        }
    }

    /// OpenCode does not run a `/command` given with --prompt, so such a
    /// brief is typed in after start.
    pub fn brief_after_start(&self, brief: &str) -> bool {
        self.name == "opencode" && brief.starts_with('/')
    }

    fn model_effort(&self, rec: &Record) -> Result<Vec<String>> {
        let mut v = Vec::new();
        if let Some(m) = &rec.model {
            let flag = if matches!(self.name, "codex" | "opencode") { "-m" } else { "--model" };
            v.extend([flag.to_string(), m.clone()]);
        }
        if let Some(e) = &rec.effort {
            match self.name {
                "claude" | "agy" => v.extend(["--effort".into(), e.clone()]),
                "codex" => v.extend(["-c".into(), format!("model_reasoning_effort=\"{e}\"")]),
                "pi" => v.extend(["--thinking".into(), e.clone()]),
                _ => bail!("{} has no effort setting", self.name),
            }
        }
        Ok(v)
    }

    /// The argv that starts a new conversation with the record's brief.
    pub fn launch(&self, rec: &Record) -> Result<Vec<String>> {
        let brief = rec.brief.clone().unwrap_or_default();
        let id = rec.session_id.clone().unwrap_or_default();
        let mut v = vec![self.program.to_string()];
        match self.name {
            "claude" | "pi" => v.extend(["--name".into(), rec.name.clone(), "--session-id".into(), id]),
            "codex" => v.push("--no-daemon".into()),
            "opencode" => v.push(".".into()),
            _ => {}
        }
        v.extend(self.model_effort(rec)?);
        v.extend(rec.args.iter().cloned());
        match self.name {
            "opencode" if self.brief_after_start(&brief) => {}
            "opencode" => v.push(format!("--prompt={brief}")),
            "agy" => v.push(format!("--prompt-interactive={brief}")),
            _ => v.extend(["--".into(), brief]),
        }
        Ok(v)
    }

    /// The argv that continues the record's conversation: by ID when known,
    /// else the newest conversation in the directory.
    pub fn resume(&self, rec: &Record) -> Vec<String> {
        let id = rec.session_id.clone().filter(|s| !s.is_empty());
        let mut v = vec![self.program.to_string()];
        match (self.name, id) {
            ("claude", Some(id)) => v.extend(["--resume".into(), id]),
            ("codex", Some(id)) => v.extend(["resume".into(), "--no-daemon".into(), id]),
            ("pi", Some(id)) => v.extend(["--session".into(), id]),
            ("opencode", Some(id)) => v.extend([".".into(), "--session".into(), id]),
            ("agy", Some(id)) => v.extend(["--conversation".into(), id]),
            ("codex", None) => v.extend(["resume".into(), "--no-daemon".into(), "--last".into()]),
            ("opencode", None) => v.extend([".".into(), "--continue".into()]),
            _ => v.push("--continue".into()),
        }
        v.extend(rec.args.iter().cloned());
        v
    }

    /// Resume continues the newest conversation in the directory.
    pub fn resumes_by_directory(&self, rec: &Record) -> bool {
        rec.session_id.as_deref().unwrap_or("").is_empty()
    }
}
