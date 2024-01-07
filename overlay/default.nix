{ inputs }: final: prev:
{
  home-assistant-custom-components = prev.home-assistant-custom-components // {
    bhyve = final.home-assistant.python.pkgs.callPackage ../packages/servers/home-assistant/custom-components/bhyve.nix { };
    eero = final.home-assistant.python.pkgs.callPackage ../packages/servers/home-assistant/custom-components/eero.nix { };
  };

  nerdifyFont = final.callPackage ./nerdify-font.nix { };
}
