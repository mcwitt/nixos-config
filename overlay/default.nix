{ inputs }:
final: prev:
let
  inherit (final) callPackage lib;

  assertNotStale =
    overrideVersion: currentVersion:
    let
      inherit (lib) assertMsg versionOlder;
    in
    assertMsg (
      !versionOlder overrideVersion currentVersion
    ) "Stale override. Update override version if it still applies.";

in
{
  emacs-unstable = prev.emacs-unstable.overrideAttrs (old: {
    patches =
      (old.patches or [ ])
      ++ final.lib.optionals final.stdenv.isDarwin [
        # https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-30
        (final.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-28/fix-window-role.patch";
          hash = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
        })
        (final.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/round-undecorated-frame.patch";
          hash = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
        })
        (final.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/system-appearance.patch";
          hash = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
        })
      ];
  });

  fish-kubectl-completions = callPackage ../packages/shells/fish/fish-kubectl-completions.nix { };

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

  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (pyFinal: pyPrev: {
      gehomesdk =
        let
          version = "2025.5.0";
        in
        assert assertNotStale version pyPrev.gehomesdk.version;
        pyPrev.gehomesdk.overridePythonAttrs (old: rec {
          inherit version;
          src = pyFinal.fetchPypi {
            inherit (old) pname;
            inherit version;
            hash = "sha256-YMw0W9EWz3KY1+aZMdtE4TRvFd9yqTHkfw0X3+ZDCfQ=";
          };
        });
    })
  ];

  nerdifyFont = callPackage ./nerdify-font.nix { };
}
