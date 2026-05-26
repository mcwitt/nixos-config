{
  config,
  gwsSkills,
  inputs,
  localSkills,
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

      skills = gwsSkills // localSkills;

      settings = {
        plugin = [ "${inputs.superpowers}/.opencode/plugins/superpowers.js" ];

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

        # Route cheap auxiliary work (title generation, summarization) to the
        # free local model instead of the paid primary provider.
        small_model = "llamaswap/Qwen3.6-27B";
      };
    };
  };
}
