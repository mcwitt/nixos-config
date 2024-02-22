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
  firefly-data-importer = final.callPackage ../packages/servers/firefly-data-importer { };

  home-assistant-custom-components = prev.home-assistant-custom-components // (
    let pythonPackages = final.home-assistant.python.pkgs;
    in {
      bhyve = pythonPackages.callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
      eero = pythonPackages.callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
    }
  );

  nerdifyFont = final.callPackage ./nerdify-font.nix { };

  python311 = overridePython prev.python311;
}
