{ inputs }: final: prev:
{
  home-assistant-custom-components = prev.home-assistant-custom-components // (
    let pythonPackages = final.home-assistant.python.pkgs;
    in {
      bhyve = pythonPackages.callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
      eero = pythonPackages.callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
    }
  );

  nerdifyFont = final.callPackage ./nerdify-font.nix { };
}
