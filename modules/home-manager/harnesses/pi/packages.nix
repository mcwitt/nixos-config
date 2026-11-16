# Third-party pi packages, vendored as store paths so they are pinned and
# reviewable instead of `pi install`ed at runtime. pi loads a local package in
# place, from its package.json `pi` manifest, and provides its own modules for
# the peer dependencies (@earendil-works/*, typebox); any other runtime
# dependency must sit in the package's own node_modules.
{ pkgs }:
let
  inherit (pkgs) fetchFromGitHub fetchurl runCommand;
in
{
  # Claude models on the Claude subscription: every model request goes through
  # the official `claude -p`, which Anthropic bills against the plan's limits
  # (support.claude.com article 15036540). pi's own Anthropic /login would draw
  # per-token extra usage instead. No releases yet; pin a main commit.
  claude-code = fetchFromGitHub {
    name = "pi-claude-code-0.1.0-unstable-2026-11-05";
    owner = "earendil-works";
    repo = "pi-claude-code";
    rev = "d5b5e04c314d3452d357174d16d10dda34daa2d2";
    hash = "sha256-86zfGr9YoM+I/PmWHDJb8AbcxhYC1m8aOeghJjvN414=";
  };

  # Claude Code-style auto mode: a model classifier decides each tool call,
  # after deterministic deny rules. It parses Bash with unbash, its one runtime
  # dependency.
  automode =
    let
      src = fetchFromGitHub {
        owner = "czottmann";
        repo = "pi-automode";
        tag = "v1.17.0";
        hash = "sha256-hC5y4ReFLsRgUWxiuGVcGsEp8akmLVDrNQ1jxF2F+uE=";
      };
      unbash = fetchurl {
        url = "https://registry.npmjs.org/unbash/-/unbash-4.0.10.tgz";
        hash = "sha512-b7zoBQvpWp0vuN5q2vK2RRBR2SvuruQAs50DApdDveBSn3eSYd84IaHodFqQIMlvY9K2VnyBUEXgwOBuGU9GBg==";
      };
    in
    runCommand "pi-automode-1.17.0" { } ''
      cp -r ${src} $out
      chmod -R u+w $out
      mkdir -p $out/node_modules/unbash
      tar xzf ${unbash} -C $out/node_modules/unbash --strip-components=1
    '';
}
