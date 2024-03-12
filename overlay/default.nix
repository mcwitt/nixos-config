{ inputs }: final: prev:
let
  inherit (final) lib;

  assertNotStale = overrideVersion: currentVersion:
    let inherit (lib) assertMsg versionOlder; in
    assertMsg
      (! versionOlder overrideVersion currentVersion)
      "Stale override. Update override version if it still applies.";

  overridePython = python: python.override (old: {
    packageOverrides = lib.composeExtensions (old.packageOverrides or (_: _: { })) (pyFinal: pyPrev: {
      # https://github.com/NixOS/nixpkgs/issues/262000
      debugpy = assert assertNotStale "1.8.0" pyPrev.debugpy.version;
        pyPrev.debugpy.overridePythonAttrs (_: { doCheck = false; });
    });
  });
in
{
  base16-rofi = final.callPackage ../packages/data/themes/base16-rofi.nix { };

  base16-tmux = final.callPackage ../packages/data/themes/base16-tmux.nix { };

  fish-kubectl-completions = final.callPackage ../packages/shells/fish/fish-kubectl-completions.nix { };

  fzf-fish = final.callPackage ../packages/shells/fish/fzf-fish.nix { };

  github-gitignore = final.callPackage ../packages/data/misc/github-gitignore.nix { };

  gitignores = final.callPackage ../packages/development/misc/gitignores.nix { };

  home-assistant-custom-components = prev.home-assistant-custom-components // (
    let ps = final.home-assistant.python.pkgs;
    in {
      bhyve = ps.callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
      eero = ps.callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
    }
  );

  nerdifyFont = final.callPackage ./nerdify-font.nix { };

  python311 = overridePython prev.python311;
}
