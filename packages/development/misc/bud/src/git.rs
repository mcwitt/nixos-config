//! Git checkouts and worktrees.

use anyhow::{bail, Context, Result};
use std::process::{Command, Stdio};

fn git(dir: &str, args: &[&str]) -> Option<String> {
    let out = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(args)
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .ok()?;
    out.status
        .success()
        .then(|| String::from_utf8_lossy(&out.stdout).trim_end().to_string())
}

pub fn toplevel(dir: &str) -> Option<String> {
    git(dir, &["rev-parse", "--show-toplevel"])
}

/// `git status --short`, or None outside a checkout.
pub fn status(dir: &str) -> Option<String> {
    git(dir, &["status", "--short"])
}

/// The path of a worktree of `repo` for `branch` (or `pr:N`), made with
/// worktrunk if needed, so the user's path template and hooks apply.
pub fn worktree(repo: &str, branch: &str, base: Option<&str>) -> Result<String> {
    let Some(top) = toplevel(repo) else { bail!("{repo} is not a Git checkout") };
    let exists = git(&top, &["rev-parse", "--verify", "--quiet", &format!("refs/heads/{branch}")]).is_some();
    let mut cmd = Command::new("wt");
    cmd.args(["-C", &top, "switch", branch, "--no-cd", "--format", "json", "--yes"]);
    if !exists && !branch.starts_with("pr:") && !branch.starts_with("mr:") {
        cmd.arg("--create");
        if let Some(b) = base {
            cmd.args(["--base", b]);
        }
    }
    let out = cmd.stdin(Stdio::null()).output().context("running wt (worktrunk)")?;
    if !out.status.success() {
        bail!("wt switch {branch} failed:\n{}", String::from_utf8_lossy(&out.stderr).trim());
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    let last = stdout.trim().lines().last().unwrap_or("{}");
    let info: serde_json::Value = serde_json::from_str(last).context("parsing wt output")?;
    let path = info["path"].as_str().context("wt output has no path")?.to_string();
    println!(
        "worktree {path} ({}, branch {})",
        info["action"].as_str().unwrap_or("?"),
        info["branch"].as_str().unwrap_or(branch)
    );
    Ok(path)
}
