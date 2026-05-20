{ inputs }:
final: prev:
let
  inherit (final) callPackage lib;
in
{
  github-gitignore = callPackage ../packages/data/misc/github-gitignore.nix { };

  gitignores = callPackage ../packages/development/misc/gitignores.nix { };

  home-assistant = prev.home-assistant.override (old: {
    packageOverrides = lib.composeExtensions (old.packageOverrides or (_: _: { })) (
      pyFinal: _:
      let
        inherit (pyFinal) callPackage;
      in
      {
        magicattr = callPackage ../packages/development/python-modules/magicattr { };
      }
    );
  });

  home-assistant-custom-components =
    prev.home-assistant-custom-components
    // (
      let
        inherit (final.home-assistant.python.pkgs) callPackage;
      in
      {
        bhyve = callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
        eero = callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
        gehome = callPackage ../packages/servers/home-assistant/custom-components/gehome.nix { };
        scheduler-component =
          callPackage ../packages/servers/home-assistant/custom-components/scheduler-component.nix
            { };
      }
    );

  home-assistant-custom-lovelace-modules = prev.home-assistant-custom-lovelace-modules // {
    scheduler-card =
      callPackage ../packages/servers/home-assistant/custom-lovelace-modules/scheduler-card
        { };
  };

  nerdifyFont = callPackage ./nerdify-font.nix { };
}
