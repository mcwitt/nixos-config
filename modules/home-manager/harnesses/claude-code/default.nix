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

        # omit the claude.ai session link from commits and PR bodies, and
        # the Co-Authored-By trailer from commits
        attribution = {
          sessionUrl = false;
          commit = "";
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
