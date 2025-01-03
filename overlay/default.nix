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
        magicattr = callPackage ../packages/development/python-modules/magicattr { };
      });
  });
in
{
  base16-rofi = callPackage ../packages/data/themes/base16-rofi.nix { };

  emacs-unstable = prev.emacs-unstable.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ final.lib.optionals final.stdenv.isDarwin [
      # https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-29
      (final.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
        hash = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
      })
      (final.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
        hash = "sha256-QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
      })
      (final.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
        hash = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
      })
      (final.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
        hash = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
      })
      (final.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
        hash = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
      })
    ];
  });

  fish-kubectl-completions = callPackage ../packages/shells/fish/fish-kubectl-completions.nix { };

  github-gitignore = callPackage ../packages/data/misc/github-gitignore.nix { };

  gitignores = callPackage ../packages/development/misc/gitignores.nix { };

  home-assistant = overridePython prev.home-assistant;

  home-assistant-custom-components = prev.home-assistant-custom-components // (
    let inherit (final.home-assistant.python.pkgs) callPackage;
    in {
      bhyve = callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
      eero = callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
      gehome = callPackage ../packages/servers/home-assistant/custom-components/gehome.nix { };
      scheduler-component = callPackage ../packages/servers/home-assistant/custom-components/scheduler-component.nix { };
    }
  );

  home-assistant-custom-lovelace-modules = prev.home-assistant-custom-lovelace-modules // {
    scheduler-card = callPackage ../packages/servers/home-assistant/custom-lovelace-modules/scheduler-card { };
  };

  nerdifyFont = callPackage ./nerdify-font.nix { };

  python311 = overridePython prev.python311;
  python312 = overridePython prev.python312;

  # wezterm rendering broken in 24.11 as of 2024-11-17
  # https://github.com/NixOS/nixpkgs/issues/336069
  wezterm =
    let
      nixpkgs = final.fetchFromGitHub {
        owner = "nixos";
        repo = "nixpkgs";
        rev = "nixos-24.05";
        hash = "sha256-df3dJApLPhd11AlueuoN0Q4fHo/hagP75LlM5K1sz9g=";
      };
      pkgs = import nixpkgs { inherit (final) system; };
    in
    pkgs.wezterm;
}
