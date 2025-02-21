{ inputs }:
final: prev:
let
  inherit (final) callPackage fetchFromGitHub lib;

  assertNotStale =
    overrideVersion: currentVersion:
    let
      inherit (lib) assertMsg versionOlder;
    in
    assertMsg (
      !versionOlder overrideVersion currentVersion
    ) "Stale override. Update override version if it still applies.";

  overridePython =
    python:
    python.override (old: {
      packageOverrides = lib.composeExtensions (old.packageOverrides or (_: _: { })) (
        pyFinal: pyPrev:
        let
          inherit (pyFinal) callPackage;
        in
        {
          magicattr = callPackage ../packages/development/python-modules/magicattr { };
        }
      );
    });
in
{
  base16-rofi = callPackage ../packages/data/themes/base16-rofi.nix { };

  emacs-unstable = prev.emacs-unstable.overrideAttrs (old: {
    patches =
      (old.patches or [ ])
      ++ final.lib.optionals final.stdenv.isDarwin [
        # https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-30
        (final.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/fix-window-role.patch";
          sha256 = "09q0qinaykx58bssw7pj8ky4j1qcgwrbgkfr8wxifl445gh5kbj6";
        })
        (final.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/round-undecorated-frame.patch";
          sha256 = "0x187xvjakm2730d1wcqbz2sny07238mabh5d97fah4qal7zhlbl";
        })
        (final.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/system-appearance.patch";
          sha256 = "1dkx8xc3v2zgnh6fpx29cf6kc5h18f9misxsdvwvy980cj0cxcwy";
        })
      ];
  });

  fish-kubectl-completions = callPackage ../packages/shells/fish/fish-kubectl-completions.nix { };

  github-gitignore = callPackage ../packages/data/misc/github-gitignore.nix { };

  gitignores = callPackage ../packages/development/misc/gitignores.nix { };

  home-assistant = overridePython prev.home-assistant;

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

  python311 = overridePython prev.python311;
  python312 = overridePython prev.python312;
}
