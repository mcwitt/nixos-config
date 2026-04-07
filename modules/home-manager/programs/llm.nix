{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.llm;

  jsonFormat = pkgs.formats.json { };

  configDir = "io.datasette.llm";

  pluginArgs = lib.genAttrs cfg.plugins (_: true);

  unwrapped = if cfg.plugins == [ ] then cfg.package else cfg.package.withPlugins pluginArgs;

  keyFileEntries = lib.attrsToList cfg.keyFiles;

  package =
    if keyFileEntries == [ ] then
      unwrapped
    else
      pkgs.symlinkJoin {
        name = "llm-wrapped";
        paths = [ unwrapped ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild =
          let
            runArgs = lib.concatMapStrings (
              { name, value }: " --run ${lib.escapeShellArg "export ${name}=\"$(cat \"${value}\")\""}"
            ) keyFileEntries;
          in
          ''
            rm $out/bin/llm
            makeWrapper ${lib.getExe unwrapped} $out/bin/llm${runArgs}
          '';
      };
in
{
  options.programs.llm = {
    enable = lib.mkEnableOption "llm - access large language models from the command line";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm;
      defaultText = "pkgs.llm";
      description = "The llm package to use.";
    };

    plugins = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "llm-anthropic" ];
      description = "List of llm plugin names to install. Must match names known to nixpkgs llm.withPlugins.";
    };

    keyFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = lib.literalExpression ''
        {
          ANTHROPIC_API_KEY = config.age.secrets.anthropic-api-key.path;
        }
      '';
      description = ''
        Mapping of environment variable names to files containing secret values.
        Each file is read at runtime and exported before invoking llm.
      '';
    };

    aliases = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        claude = "claude-sonnet-4-5";
      };
      description = "Model aliases, written to aliases.json.";
    };

    defaultModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "claude-opus-4-6";
      description = "Default model, written to default_model.txt.";
    };

    extraOpenAIModels = lib.mkOption {
      type = lib.types.listOf jsonFormat.type;
      default = [ ];
      example = [
        {
          model_id = "deepseek-v3";
          model_name = "deepseek-chat";
          api_base = "https://api.deepseek.com/v1";
          api_key_name = "DEEPSEEK_API_KEY";
        }
      ];
      description = "Extra OpenAI-compatible model configurations, written to extra-openai-models.yaml.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ package ];

    xdg.configFile = lib.mkMerge [
      (lib.mkIf (cfg.defaultModel != null) {
        "${configDir}/default_model.txt".text = cfg.defaultModel;
      })
      (lib.mkIf (cfg.aliases != { }) {
        "${configDir}/aliases.json".text = builtins.toJSON cfg.aliases;
      })
      (lib.mkIf (cfg.extraOpenAIModels != [ ]) {
        # JSON is valid YAML; llm requires this filename
        "${configDir}/extra-openai-models.yaml".text = builtins.toJSON cfg.extraOpenAIModels;
      })
    ];
  };
}
