{ pkgs, ... }: {
  home.packages = with pkgs; [ black mypy python3Packages.flake8 ];
}
