{ inputs }:
final: prev:
let
  inherit (final) callPackage lib;
in
{
  # nixos-26.05's manually packaged Ghostel shadows the current package from
  # emacs-overlay. Backport the package from emacs-overlay's nixpkgs input so
  # Ghostel's native module is still built declaratively rather than downloaded
  # on first use.
  emacsPackagesFor =
    emacs:
    let
      emacsPackages = prev.emacsPackagesFor emacs;
    in
    assert lib.assertMsg (lib.versionOlder emacsPackages.ghostel.version "0.51.0")
      "nixos-26.05 now has Ghostel >= 0.51.0; drop this backport.";
    emacsPackages.overrideScope (
      emacsFinal: _: {
        ghostel =
          emacsFinal.callPackage
            "${inputs.emacs-overlay.inputs.nixpkgs}/pkgs/applications/editors/emacs/elisp-packages/manual-packages/ghostel/package.nix"
            { };
      }
    );

  # numtide/llm-agents.nix dropped its `overlays.default` output in the
  # 2026-07 restructure; recreate it here so `pkgs.llm-agents.<pkg>` keeps
  # working. Expose the flake's packages as-is, built against its own tested
  # nixpkgs pin rather than our stable package set. Do not use the upstream
  # shared-nixpkgs overlay here: it would build against our stable nixpkgs.
  llm-agents = inputs.llm-agents.packages.${final.stdenv.hostPlatform.system};

  agy-acp-server = callPackage ../packages/development/misc/agy-acp-server.nix { };

  # MCP profiles are mutable state, but nixpkgs points PLAYWRIGHT_BROWSERS_PATH
  # at the read-only store. Keep the browser binaries there and use Playwright's
  # normal per-user cache for profiles, preserving its per-project names.
  # Drop when https://github.com/microsoft/playwright/issues/40892 is fixed.
  playwright-mcp = prev.playwright-mcp.override {
    playwright-test = prev.playwright-test.overrideAttrs (old: {
      postInstall = (old.postInstall or "") + ''
        profile_factory="$out/lib/node_modules/playwright-core/lib/tools/mcp/browserFactory.js"
        chmod u+w "$profile_factory"
        substituteInPlace "$profile_factory" \
          --replace-fail 'process.env.PWMCP_PROFILES_DIR_FOR_TEST ?? import_registry.registryDirectory' \
                         'process.env.PWMCP_PROFILES_DIR_FOR_TEST ?? import_registry.defaultRegistryDirectory'
      '';
    });
  };

  # claude-code 2.1.206+ emits command_lifecycle frames that
  # claude-agent-acp < 0.59.0 treats as errors on every prompt.
  # Drop once the nixpkgs pin provides >= 0.76.0:
  #   nix eval --raw --impure --expr '
  #     let flake = builtins.getFlake (toString ./.);
  #     in flake.inputs.nixpkgs.legacyPackages.${builtins.currentSystem}.claude-agent-acp.version
  #   '
  claude-agent-acp = callPackage ../packages/development/misc/claude-agent-acp.nix { };

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
          version = "2026.8.0";
          src = oldAttrs.src.override {
            inherit version;
            hash = "sha256-4WGkodI608LmlHpzYfPIrexZBNAsImmj2B55+WJgq2E=";
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
        inherit (final.home-assistant.python3Packages) callPackage;
      in
      {
        bhyve = callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
        eero = callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
        gehome = callPackage ../packages/servers/home-assistant/custom-components/gehome.nix { };
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
