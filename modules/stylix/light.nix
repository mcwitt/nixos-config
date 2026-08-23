{ lib, ... }:
{
  # specialisation is a NixOS feature; nix-darwin has no equivalent, so this
  # light variant is wired into makeNixosSystem only (not makeDarwinSystem).
  specialisation.light.configuration.stylix = {
    # Hand-authored Base16 projection of the ef-melissa-light Emacs theme.
    # Keep syntax colors readable against bg-main rather than using the
    # theme's low-contrast border/background colors in foreground slots.
    base16Scheme = lib.mkForce {
      base00 = "#fff6d8"; # bg-main
      base01 = "#f5e9cb"; # bg-dim
      base02 = "#c7b7a6"; # bg-active / selection
      base03 = "#68708a"; # fg-dim / comments
      base04 = "#80431a"; # fg-alt
      base05 = "#484431"; # fg-main
      base06 = "#403328"; # fg-mode-line-active
      base07 = "#403328"; # strongest foreground (Base16 compatibility)
      base08 = "#ba2d2f"; # red
      base09 = "#ba5205"; # yellow-warmer / orange
      base0A = "#a26310"; # yellow
      base0B = "#007a0a"; # green
      base0C = "#3f60af"; # cyan
      base0D = "#375cc6"; # blue
      base0E = "#aa3e74"; # magenta
      base0F = "#c74400"; # red-warmer / rust
    };

    polarity = lib.mkForce "light";
  };
}
