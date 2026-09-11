{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.agent-identity;
  instructions = builtins.readFile ./instructions.md;
  guard = pkgs.writeShellApplication {
    name = "agent-gh-guard";
    runtimeInputs = [
      pkgs.jq
      pkgs.shfmt
    ];
    text = builtins.readFile ./guard.sh;
  };
  hooks.PreToolUse = [
    {
      matcher = "^Bash$";
      hooks = [
        {
          type = "command";
          command = lib.getExe guard;
        }
      ];
    }
  ];
in
{
  options.profiles.agent-identity = {
    enable = lib.mkEnableOption "agent GitHub identity commands and instructions";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      (pkgs.writeShellApplication {
        name = "personal-gh";
        text = ''
          exec ${lib.getExe pkgs.gh} "$@"
        '';
      })
    ];

    programs.claude-code = {
      context = instructions;
      settings.hooks = hooks;
    };
    programs.codex = {
      context = instructions;
      settings.hooks = hooks;
    };
    programs.opencode.context = instructions;
    # Pi is installed by the harness bundle; it has no Home Manager program option.
    home.file.".pi/agent/AGENTS.md" = lib.mkIf (config.harnesses.enable or false) {
      text = instructions;
    };
  };
}
