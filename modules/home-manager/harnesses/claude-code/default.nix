{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  settingsFile = "${config.programs.claude-code.configDir}/settings.json";
  statuslineScript = import ./statusline.nix { inherit config pkgs; };
  tmuxStateHooks = import ./tmux-state.nix { inherit config pkgs; };
in
{
  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.sox # for voice mode
    ];

    programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        effortLevel = "high";

        # Fullscreen is still opt-in for existing accounts and some terminals.
        tui = "fullscreen";

        # always a terminal frame for Ctrl+g: emacsclient-auto's display
        # sniffing misfires inside a multiplexer attached over SSH
        env.VISUAL = "emacsclient -t";

        # omit every attribution Claude Code adds: the claude.ai session link,
        # the Co-Authored-By commit trailer and the "Generated with" PR line
        attribution = {
          sessionUrl = false;
          commit = "";
          pr = "";
        };

        permissions.defaultMode = "auto";
        skipAutoPermissionPrompt = true;
        editorMode = "vim";
        voiceEnabled = true;
        theme = if config.stylix.polarity == "dark" then "dark" else "light";
        statusLine = {
          type = "command";
          command = "${statuslineScript}";
        };

        hooks = tmuxStateHooks;
      };

      plugins = [ "${pkgs.worktrunk.src}" ];

      inherit (cfg) skills;
    };

    # Claude Code persists settings; the pinned Home Manager module still only
    # provides a read-only settings.json, with no mutable settings option.
    home.file.${settingsFile}.enable = lib.mkForce false;

    home.activation.claudeCodeWritableSettings =
      let
        target = "${config.home.homeDirectory}/${config.home.file.${settingsFile}.target}";
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run rm -f "${target}"
        run install -D -m600 ${config.home.file.${settingsFile}.source} "${target}"
      '';
  };
}
