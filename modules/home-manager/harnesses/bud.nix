# bud: launch agent sessions in tmux as a tree, and supervise them. The
# shared `bud` skill, the `bud` helper, the hook that reminds a parent or a
# child of its place in the tree, and the prefix S tree picker.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  bud = lib.getExe pkgs.bud;
  context = {
    type = "command";
    command = "${bud} _context";
    timeout = 5;
  };
in
{
  config = lib.mkIf cfg.enable {
    harnesses.skills.bud = ./skills/bud;
    home.packages = [ pkgs.bud ];

    # On compaction too: that is when a parent forgets its children.
    programs.claude-code.settings.hooks.SessionStart = [
      {
        matcher = "^(startup|resume|compact|clear)$";
        hooks = [ context ];
      }
    ];
    programs.codex.settings.hooks.SessionStart = [
      {
        matcher = "startup|resume|clear|compact";
        hooks = [ context ];
      }
    ];

    # display-popup does not expand formats in its command; run-shell does.
    programs.tmux.extraConfig = ''
      bind S run-shell -b '${bud} pick #{q:client_name}'
    '';
  };
}
