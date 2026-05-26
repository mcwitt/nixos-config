{
  config,
  gwsSkills,
  lib,
  ...
}:
let
  cfg = config.profiles.base;
in
{
  config = lib.mkIf cfg.enable {
    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;

      skills = gwsSkills;

      settings.provider.llamaswap = {
        npm = "@ai-sdk/openai-compatible";
        name = "llama-swap (local)";
        # satori serves llama-swap on the personal tailnet; reachable by MagicDNS
        # name from every host (including satori itself).
        options.baseURL = "http://satori:11343/v1";
        models = {
          # key MUST match the llama-swap model name in the private repo's
          # hosts/satori/configuration/llama-swap.nix
          "Qwen3.6-27B" = {
            name = "Qwen3.6 27B (local)";
          };
        };
      };
    };
  };
}
