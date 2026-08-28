{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.coding-agent;
in
{
  options.coding-agent = {
    enable = lib.mkEnableOption "an interactive coding-agent environment";

    git = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Coding Agent";
        description = "Git commit author name for the coding-agent identity.";
      };

      email = lib.mkOption {
        type = lib.types.str;
        default = "coding-agent@localhost.invalid";
        description = "Git commit author email for the coding-agent identity.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    harnesses.enable = true;

    home.packages = with pkgs; [
      fd
      tree
    ];

    languages = {
      nix.enable = true;
      python.enable = true;
      shell.enable = true;
    };

    programs = {
      bash.enable = true;
      bat.enable = true;

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fish.enable = true;
      fzf.enable = true;
      gh.enable = true;

      git = {
        enable = true;
        settings = {
          init.defaultBranch = "main";
          merge.ff = "only";
          pull.rebase = true;
          user = {
            inherit (cfg.git) name email;
          };
        };
      };

      home-manager.enable = true;
      jq.enable = true;
      ripgrep.enable = true;
      zellij.enable = true;
    };
  };
}
