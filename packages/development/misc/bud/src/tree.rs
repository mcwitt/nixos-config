//! The tree: records give the edges, tmux gives liveness.

use crate::record::{self, Record};
use crate::tmux::{Pane, Snapshot};
use std::collections::{HashMap, HashSet};

pub struct World {
    pub recs: HashMap<String, Record>,
    pub snap: Snapshot,
}

impl World {
    pub fn read() -> World {
        World { recs: record::all(), snap: Snapshot::read() }
    }

    /// The agent pane of a session: the recorded pane if it is still in
    /// that session; for a session without a record, its first pane.
    pub fn agent_pane(&self, name: &str) -> Option<&Pane> {
        if !self.snap.sessions.contains_key(name) {
            return None;
        }
        match self.recs.get(name) {
            Some(rec) => self.snap.pane(name, rec.pane.as_deref()?),
            None => self.snap.session_panes(name).into_iter().next(),
        }
    }

    /// A session is live when it exists and its agent pane is not dead. A
    /// session whose recorded pane is stale still counts: it is there, but
    /// bud refuses to send it input until the pane is recorded again.
    pub fn is_live(&self, name: &str) -> bool {
        self.snap.sessions.contains_key(name) && !self.agent_pane(name).is_some_and(|p| p.dead)
    }

    /// The nearest ancestor of `name` for which `keep` holds.
    fn nearest(&self, name: &str, keep: &dyn Fn(&str) -> bool) -> Option<String> {
        let mut seen = HashSet::from([name.to_string()]);
        let mut p = self.recs.get(name)?.parent.clone();
        while let Some(n) = p {
            if !seen.insert(n.clone()) {
                return None;
            }
            if keep(&n) {
                return Some(n);
            }
            p = self.recs.get(&n).and_then(|r| r.parent.clone());
        }
        None
    }

    /// The nearest live ancestor.
    pub fn effective_parent(&self, name: &str) -> Option<String> {
        self.nearest(name, &|n| self.is_live(n))
    }

    /// Live sessions whose effective parent is `name`.
    pub fn children(&self, name: &str) -> Vec<String> {
        let mut v: Vec<String> = self
            .recs
            .keys()
            .filter(|n| n.as_str() != name && self.is_live(n))
            .filter(|n| self.effective_parent(n).as_deref() == Some(name))
            .cloned()
            .collect();
        self.sort(&mut v);
        v
    }

    /// The live descendants of `name`, deepest first.
    pub fn descendants(&self, name: &str) -> Vec<String> {
        let mut out = Vec::new();
        for c in self.children(name) {
            out.extend(self.descendants(&c));
            out.push(c);
        }
        out
    }

    fn started(&self, name: &str) -> u64 {
        match (self.recs.get(name), self.snap.sessions.get(name)) {
            (Some(r), _) if r.launched_at > 0 => r.launched_at,
            (_, Some(s)) => s.created,
            _ => 0,
        }
    }

    fn sort(&self, v: &mut [String]) {
        v.sort_by(|a, b| self.started(a).cmp(&self.started(b)).then(a.cmp(b)));
    }

    /// `nodes` in tree order with their depths. A node's parent is its
    /// nearest recorded ancestor among `nodes`. With `root`, only that
    /// node's subtree.
    pub fn layout(&self, nodes: &HashSet<String>, root: Option<&str>) -> Vec<(String, usize)> {
        let mut kids: HashMap<Option<String>, Vec<String>> = HashMap::new();
        for n in nodes {
            let parent = self.nearest(n, &|a| nodes.contains(a));
            kids.entry(parent).or_default().push(n.clone());
        }
        for v in kids.values_mut() {
            self.sort(v);
        }
        let mut out = Vec::new();
        fn walk(
            kids: &HashMap<Option<String>, Vec<String>>,
            n: &str,
            depth: usize,
            out: &mut Vec<(String, usize)>,
        ) {
            out.push((n.to_string(), depth));
            for c in kids.get(&Some(n.to_string())).into_iter().flatten() {
                walk(kids, c, depth + 1, out);
            }
        }
        match root {
            Some(r) => walk(&kids, r, 0, &mut out),
            None => {
                for r in kids.get(&None).into_iter().flatten() {
                    walk(&kids, r, 0, &mut out);
                }
            }
        }
        out
    }

    /// The state to show: the agent's own report, `exited(N)` for a pane
    /// that failed, `closed` for a session that is gone.
    pub fn state(&self, name: &str) -> String {
        match self.agent_pane(name) {
            Some(p) if p.dead => format!("exited({})", p.dead_status),
            Some(p) if !p.state.is_empty() => p.state.clone(),
            Some(_) => "-".into(),
            None if self.snap.sessions.contains_key(name) => "-".into(),
            None => "closed".into(),
        }
    }
}
