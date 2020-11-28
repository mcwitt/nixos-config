{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.darwin-home;
in
{
  options.profiles.darwin-home.enable = mkEnableOption "Darwin home profile";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.lorri ];

    profiles.default.enable = true;

    # HACK fix for darwin/fish PATH ordering; see https://github.com/LnL7/nix-darwin/issues/122
    programs.fish.shellInit = ''
      for p in /run/current-system/sw/bin ${config.home.homeDirectory}/.nix-profile/bin
          if not contains $p $fish_user_paths
              set -g fish_user_paths $p $fish_user_paths
          end
      end
    '';

    programs.git.ignores =
      pkgs.mypkgs.gitignore.ghGitIgnoreLines "Global/macOS";
  };
}
