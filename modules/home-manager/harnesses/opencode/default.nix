{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
in
{
  config = lib.mkIf cfg.enable {
    stylix.targets.opencode.enable = false;

    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;

      skills = cfg.skills;

      settings = {
        plugin = [
          "${inputs.superpowers}/.opencode/plugins/superpowers.js"
          "${pkgs.worktrunk.src}/dev/opencode-plugin.ts"
        ];

        provider.llamaswap = {
          npm = "@ai-sdk/openai-compatible";
          name = "llama-swap (local)";
          options.baseURL = "http://satori-ts:8080/v1";
          models = {
            "Qwen3.6-27B" = {
              name = "Qwen3.6 27B (local)";
              limit = {
                context = 131072; # must match -c in the private repo's llama-swap.nix
                output = 8192;
              };
            };
          };
        };
      };

      tui.theme = "system";
    };
  };
}
