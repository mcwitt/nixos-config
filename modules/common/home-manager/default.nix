{ inputs, ... }:
{
  imports = [
    inputs.base16.homeManagerModule
    ./languages
    ./profiles
    ./programs
    ./tools
  ];

  scheme = "${inputs.base16-schemes}/nord.yaml";
}
