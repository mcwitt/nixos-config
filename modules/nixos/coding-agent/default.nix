{
  config,
  lib,
  options,
  pkgs,
  ...
}:
let
  cfg = config.coding-agent;
  hasHomeManager = options ? home-manager.users;
in
{
  options.coding-agent = {
    enable = lib.mkEnableOption "an isolated coding-agent user";

    username = lib.mkOption {
      type = lib.types.str;
      default = "agent";
      description = "Name of the coding-agent user and its primary group.";
    };

    homeDirectory = lib.mkOption {
      type = lib.types.str;
      default = "/home/${cfg.username}";
      defaultText = lib.literalExpression ''"/home/''${config.coding-agent.username}"'';
      description = "Home directory of the coding-agent user.";
    };

    homeStateVersion = lib.mkOption {
      type = lib.types.str;
      default = config.system.stateVersion;
      defaultText = lib.literalExpression "config.system.stateVersion";
      description = "Home Manager state version for the coding-agent user.";
    };

    extraGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Supplementary groups granted to the coding-agent user.";
    };

    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "SSH public keys authorized to log in as the coding-agent user.";
    };

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

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        assertions = [
          {
            assertion = hasHomeManager;
            message = "coding-agent requires the Home Manager NixOS module";
          }
        ];

        users.groups.${cfg.username} = { };

        users.users.${cfg.username} = {
          isNormalUser = true;
          description = "Coding agent";
          extraGroups = lib.mkForce cfg.extraGroups;
          group = cfg.username;
          hashedPassword = "!";
          home = cfg.homeDirectory;
          homeMode = "0700";
          shell = pkgs.fish;
          openssh.authorizedKeys.keys = cfg.authorizedKeys;
        };
      }

      (lib.optionalAttrs hasHomeManager {
        home-manager.users.${cfg.username} = {
          imports = [ ../../home-manager ];

          home = {
            username = cfg.username;
            homeDirectory = cfg.homeDirectory;
            stateVersion = cfg.homeStateVersion;
          };

          profiles.base.enable = lib.mkForce false;

          coding-agent = {
            enable = true;
            inherit (cfg) git;
          };
        };
      })
    ]
  );
}
