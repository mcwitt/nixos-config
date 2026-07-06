{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  piCfg = config.harnesses.pi;

  # Sourced from the numtide/llm-agents.nix overlay (already applied in
  # flake.nix) rather than nixpkgs: it tracks pi's near-daily releases, whereas
  # nixpkgs lags by weeks. Bump with `nix flake update llm-agents`.
  piPkg = pkgs.llm-agents.pi;

  # pi has no home-manager module (only the nixpkgs package), so we wire it by
  # hand: install the package, optionally wrapped to export provider API keys
  # from files at runtime and to pin a default provider/model.
  wrapped = pkgs.symlinkJoin {
    name = "pi-${piPkg.version}";
    inherit (piPkg) version;
    paths = [ piPkg ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm $out/bin/pi
      makeWrapper ${lib.getExe' piPkg "pi"} $out/bin/pi \
        ${
          lib.concatStringsSep " " (
            lib.mapAttrsToList (
              env: file: "--run ${lib.escapeShellArg "export ${env}=\"$(cat \"${file}\")\""}"
            ) piCfg.envKeyFiles
          )
        } \
        ${lib.optionalString (
          piCfg.extraArgs != [ ]
        ) "--add-flags ${lib.escapeShellArg (lib.concatStringsSep " " piCfg.extraArgs)}"}
    '';
  };

  needsWrapper = piCfg.envKeyFiles != { } || piCfg.extraArgs != [ ];
  package = if needsWrapper then wrapped else piPkg;

  # pi-acp spawns `pi --mode rpc` via a bare PATH lookup (see
  # src/pi-rpc/command.ts in svkozak/pi-acp). Emacs launched from the GUI
  # doesn't inherit `~/.nix-profile/bin` on exec-path, so the spawn fails
  # with ENOENT and prompts silently go nowhere. Wrap the adapter to prepend
  # the final pi binary's bin dir to PATH, so it resolves regardless of the
  # Emacs process environment.
  acpPackage = pkgs.symlinkJoin {
    name = "pi-acp-wrapped";
    paths = [ pkgs.pi-acp ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/pi-acp --prefix PATH : ${lib.makeBinPath [ package ]}
    '';
  };

  # Stylix-matched theme, generated from the base16 palette so pi tracks the
  # system scheme. Chrome (accent/borders/titles/bullets) uses base06, the
  # canonical desktop focus/accent slot; content uses the semantic base16 roles.
  haveStylix = (config.lib ? stylix) && (config.lib.stylix ? colors);
  colors = config.lib.stylix.colors;
  c = name: "#${colors.${name}}";
  stylixTheme = {
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
  options.harnesses.pi = {
    envKeyFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        OPENROUTER_API_KEY = "/run/agenix/openrouter-api-key";
      };
      description = ''
        Map of environment variable name to a file containing its value. When
        non-empty, the pi package is wrapped to export each variable (read from
        its file at runtime) before exec, so providers can authenticate without
        an interactive `/login` (pi resolves API keys from the environment).
      '';
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "--provider"
        "openrouter"
        "--model"
        "deepseek/deepseek-v4-pro"
      ];
      description = ''
        Flags appended to every `pi` invocation via the wrapper, e.g. to pin a
        default provider/model so the first launch lands on a configured model
        rather than pi's built-in default (which may have no key).
      '';
    };

    modelsJson = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = ''
        Contents of `~/.pi/agent/models.json` (custom providers/models). When
        non-empty it is written declaratively; pi reloads it when you open
        `/model`. Use to surface provider slugs pi does not ship built-in. pi
        only reads this file (it persists credentials to auth.json instead), so
        a read-only nix-store symlink is safe.
      '';
    };

    finalPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        The final `pi` package, wrapped per {option}`envKeyFiles` and
        {option}`extraArgs` when applicable. Consumers that spawn `pi` as a
        subprocess (e.g. `pi-acp`) should use this rather than
        `pkgs.llm-agents.pi` so they resolve the same env-injected binary.
      '';
    };

    acpPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        `pkgs.pi-acp` wrapped to find {option}`finalPackage` on its `PATH`,
        so the adapter can spawn `pi --mode rpc` without relying on the
        user-level profile being on Emacs's `exec-path`.
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        home.packages = [ package ];
        harnesses.pi.finalPackage = package;
        harnesses.pi.acpPackage = acpPackage;
      }
      (lib.mkIf (piCfg.modelsJson != { }) {
        home.file.".pi/agent/models.json".source =
          (pkgs.formats.json { }).generate "pi-models.json"
            piCfg.modelsJson;
      })
      (lib.mkIf haveStylix {
        # The theme file is read-only (pi only reads themes/ and hot-reloads).
        home.file.".pi/agent/themes/stylix.json".source =
          (pkgs.formats.json { }).generate "pi-stylix-theme.json"
            stylixTheme;

        # Activate it via settings.json. pi rewrites settings.json on
        # interactive /settings changes, so we seed it writably (jq-merge,
        # preserving other keys) rather than symlinking it read-only. The
        # stylix theme is re-pinned on every switch.
        home.activation.piStylixTheme = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          PI_SETTINGS="${config.home.homeDirectory}/.pi/agent/settings.json"
          run mkdir -p "$(dirname "$PI_SETTINGS")"
          if [ -e "$PI_SETTINGS" ]; then
            _pi_settings="$(${lib.getExe pkgs.jq} '.theme = "stylix"' "$PI_SETTINGS")"
          else
            _pi_settings='{"theme":"stylix"}'
          fi
          run install -m644 /dev/stdin "$PI_SETTINGS" <<< "$_pi_settings"
        '';
      })
    ]
  );
}
