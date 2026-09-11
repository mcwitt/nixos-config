"""Behavioral tests for the PreToolUse hook; no commands are executed."""

import json
import subprocess
import sys
import unittest

GUARD = sys.argv.pop(1)


class GuardTests(unittest.TestCase):
    def check_commands(self, commands, expected):
        for command in commands:
            with self.subTest(command=command):
                result = subprocess.run(
                    ["bash", "-euo", "pipefail", GUARD],
                    input=json.dumps({"tool_input": {"command": command}}),
                    text=True,
                    capture_output=True,
                    check=False,
                )
                self.assertEqual(result.returncode, expected, result.stderr)
                if expected == 2:
                    self.assertIn("Use agent-gh", result.stderr)

    def test_blocks_github_commands(self):
        self.check_commands([
            "gh pr create", "  gh api user", "/usr/bin/gh pr list",
            "'gh' api user", '"gh" api user', "g'h' api user",
            "command gh api user", "command -p -- gh api user",
            "GH_PAGER=cat gh pr list", "true && gh pr list",
            "false || gh pr list", "true; gh pr list", "true\ngh pr list",
            "gh pr list | cat", "(gh pr list)", 'echo "$(gh api user)"',
            "cat <(gh api user)", "if true; then gh pr list; fi",
        ], 2)

    def test_allows_wrappers_and_noncommands(self):
        self.check_commands([
            "agent-gh pr create", "personal-gh pr create",
            "command agent-gh api user", "/some/path/personal-gh api user",
            "echo gh", "rg gh README.md", "echo 'text; gh api user'",
            '# gh api user\necho ok', "cat <<'EOF'\ngh api user\nEOF",
            "echo '$(gh api user)'", "git status", "", "command -v gh",
        ], 0)

    def test_documented_limits_are_advisory(self):
        self.check_commands([
            'tool=gh; "$tool" api user', "bash -c 'gh api user'",
            "env GH_PAGER=cat gh pr list", "if [", # invalid syntax
        ], 0)


if __name__ == "__main__":
    unittest.main()
