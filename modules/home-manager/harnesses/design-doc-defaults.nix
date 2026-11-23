# design-doc-defaults: one user's settings for the design-doc skill, and the
# commands that the skill uses. The skill is a default: a profile replaces
# it by defining `harnesses.skills.design-doc-defaults`, and removes it with
# `harnesses.designDoc.defaults.enable = false`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
in
{
  options.harnesses.designDoc.defaults.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = ''
      Whether to install the `design-doc-defaults` skill and its commands.
      `harnesses.skills` cannot drop a single name, so this option is how a
      profile removes the skill. The `design-doc` skill then asks the user
      for each setting.
    '';
  };

  config = lib.mkIf (cfg.enable && cfg.designDoc.defaults.enable) {
    harnesses.skills.design-doc-defaults = lib.mkDefault ./skills/design-doc-defaults;
    home.packages = [ pkgs.design-doc-tools ];
  };
}
