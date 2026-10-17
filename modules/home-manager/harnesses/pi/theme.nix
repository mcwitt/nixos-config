{
  config,
  lib,
  pkgs,
  ...
}:
let
  jsonFormat = pkgs.formats.json { };
  haveStylix = (config.lib ? stylix) && (config.lib.stylix ? colors);
  c = name: "#${config.lib.stylix.colors.${name}}";

  # Chrome uses base06, the desktop focus/accent slot; content uses the
  # semantic base16 roles.
  theme = {
    name = "stylix";
    colors = {
      # Core UI
      accent = c "base06";
      border = c "base03";
      borderAccent = c "base06";
      borderMuted = c "base02";
      success = c "base0B";
      error = c "base08";
      warning = c "base0A";
      muted = c "base03";
      dim = c "base04";
      text = c "base05";
      thinkingText = c "base03";
      # Backgrounds & content
      selectedBg = c "base02";
      userMessageBg = c "base01";
      userMessageText = c "base05";
      customMessageBg = c "base01";
      customMessageText = c "base05";
      customMessageLabel = c "base06";
      toolPendingBg = c "base01";
      toolSuccessBg = c "base01";
      toolErrorBg = c "base01";
      toolTitle = c "base06";
      toolOutput = c "base05";
      # Markdown
      mdHeading = c "base0D";
      mdLink = c "base0D";
      mdLinkUrl = c "base0C";
      mdCode = c "base0C";
      mdCodeBlock = c "base05";
      mdCodeBlockBorder = c "base03";
      mdQuote = c "base03";
      mdQuoteBorder = c "base03";
      mdHr = c "base03";
      mdListBullet = c "base06";
      # Tool diffs
      toolDiffAdded = c "base0B";
      toolDiffRemoved = c "base08";
      toolDiffContext = c "base03";
      # Syntax (standard base16 roles)
      syntaxComment = c "base03";
      syntaxKeyword = c "base0E";
      syntaxFunction = c "base0D";
      syntaxVariable = c "base08";
      syntaxString = c "base0B";
      syntaxNumber = c "base09";
      syntaxType = c "base0A";
      syntaxOperator = c "base05";
      syntaxPunctuation = c "base05";
      # Thinking levels (cool -> warm as effort rises)
      thinkingOff = c "base03";
      thinkingMinimal = c "base0C";
      thinkingLow = c "base0B";
      thinkingMedium = c "base0A";
      thinkingHigh = c "base09";
      thinkingXhigh = c "base08";
      # Bash mode
      bashMode = c "base09";
    };
  };
in
{
  config = lib.mkIf (config.harnesses.enable && haveStylix) {
    home.file.".pi/agent/themes/stylix.json".source = jsonFormat.generate "pi-stylix-theme.json" theme;

    # Pi rewrites settings.json on interactive /settings changes. The pinned
    # home-manager has no Pi module, so merge the theme into a writable file,
    # preserving runtime settings. Revisit when home-manager gains Pi support.
    home.activation.piStylixTheme = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      PI_SETTINGS="${config.home.homeDirectory}/.pi/agent/settings.json"
      run mkdir -p "$(dirname "$PI_SETTINGS")"
      if [ -e "$PI_SETTINGS" ]; then
        _pi_settings="$(${lib.getExe pkgs.jq} '.theme = "stylix"' "$PI_SETTINGS")"
      else
        _pi_settings='{"theme":"stylix"}'
      fi
      _pi_tmp="$(${lib.getExe' pkgs.coreutils "mktemp"})"
      printf '%s\n' "$_pi_settings" > "$_pi_tmp"
      run install -m644 "$_pi_tmp" "$PI_SETTINGS"
      rm -f "$_pi_tmp"
    '';
  };
}
