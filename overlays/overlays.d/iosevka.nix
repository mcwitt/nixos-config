_: super:
let
  pkgs = import
    (super.fetchFromGitHub
      {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "0db8a06b90973dff7427cf165666cfb9c16c95d5";
        sha256 = "sha256-jEleeuUNNKkix5NEjqp9rdNf9p47RbyW8c0fAKjie6A=";
      })
    { };
in
{
  iosevka-custom = pkgs.iosevka.override {
    privateBuildPlan = {
      family = "Iosevka Custom";
      spacing = "normal";
      serifs = "sans";
      ligations.inherits = "haskell";
    };
    set = "custom";
  };
}
