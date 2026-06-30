{ inputs }:
final: prev:
let
  inherit (final) callPackage lib;
in
{
  github-gitignore = callPackage ../packages/data/misc/github-gitignore.nix { };

  gitignores = callPackage ../packages/development/misc/gitignores.nix { };

  pi-acp = callPackage ../packages/development/misc/pi-acp { };

  home-assistant = prev.home-assistant.override (old: {
    packageOverrides = lib.composeExtensions (old.packageOverrides or (_: _: { })) (
      pyFinal: pyPrev:
      let
        inherit (pyFinal) callPackage;
      in
      {
        magicattr = callPackage ../packages/development/python-modules/magicattr { };

        gehomesdk = pyPrev.gehomesdk.overridePythonAttrs (oldAttrs: rec {
          version = "2026.5.4";
          src = oldAttrs.src.override {
            inherit version;
            hash = "sha256-zKYe7vIXSFbtTqaCLEAHQvuDRGGXqorqfFqVVpBWuJs=";
          };
          dependencies = oldAttrs.dependencies ++ [ pyFinal.beautifulsoup4 ];
        });
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

  # Bump llm-anthropic past our pinned nixpkgs (0.25) so `llm` knows about
  # claude-opus-4-8 (added upstream in 0.25.1; alias claude-opus-4.8). `pkgs.llm`
  # resolves withPlugins entries from python3Packages, so override there.
  # Drop this once the nixpkgs pin advances past commit ce95dc8 (2026-06-06,
  # "python3Packages.llm-anthropic: 0.25 -> 0.25.1"):
  #   nix eval --raw 'nixpkgs#python3Packages.llm-anthropic.version'  # expect >= 0.25.1
  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (_: pyPrev: {
      llm-anthropic = pyPrev.llm-anthropic.overridePythonAttrs (oldAttrs: rec {
        version = "0.25.1";
        src = oldAttrs.src.override {
          tag = version;
          hash = "sha256-b9XnPxKDGsiy20Me70sYrkMVO36OF3EwWOHLyEd5z4E=";
        };
      });
    })
  ];
}
