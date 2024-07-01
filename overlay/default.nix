{ inputs }: final: prev:
let
  inherit (final) callPackage fetchFromGitHub lib;

  assertNotStale = overrideVersion: currentVersion:
    let inherit (lib) assertMsg versionOlder; in
    assertMsg
      (! versionOlder overrideVersion currentVersion)
      "Stale override. Update override version if it still applies.";

  overridePython = python: python.override (old: {
    packageOverrides = lib.composeExtensions (old.packageOverrides or (_: _: { })) (pyFinal: pyPrev:
      let inherit (pyFinal) callPackage;
      in {
        aiosolaredge = callPackage ../packages/development/python-modules/aiosolaredge { };
      });
  });
in
{
  base16-rofi = callPackage ../packages/data/themes/base16-rofi.nix { };

  base16-tmux = callPackage ../packages/data/themes/base16-tmux.nix { };

  cfdyndns =
    let
      nixpkgs = fetchFromGitHub {
        owner = "mcwitt";
        repo = "nixpkgs";
        rev = "068def2ae6c29ab4a5085f81df1abcdcfb906907";
        hash = "sha256-K6zi7qJHMM4povtbQIiX7u/WY/rS+a8z/3GBRY3KRIQ=";
      };
      pkg = callPackage "${nixpkgs}/pkgs/applications/networking/dyndns/cfdyndns" { };
    in
    assert assertNotStale pkg.version prev.cfdyndns.version; pkg;

  fish-kubectl-completions = callPackage ../packages/shells/fish/fish-kubectl-completions.nix { };

  fzf-fish = callPackage ../packages/shells/fish/fzf-fish.nix { };

  github-gitignore = callPackage ../packages/data/misc/github-gitignore.nix { };

  gitignores = callPackage ../packages/development/misc/gitignores.nix { };

  home-assistant-custom-components = prev.home-assistant-custom-components // (
    let inherit (final.home-assistant.python.pkgs) callPackage;
    in {
      bhyve = callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
      eero = callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
    }
  );

  nerdifyFont = callPackage ./nerdify-font.nix { };

  python311 = overridePython prev.python311;
}
