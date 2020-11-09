{ pkgs, ... }:
let ghcEnv = pkgs.haskellPackages.ghcWithHoogle pkgs.mypkgs.myHaskellPackages;

in
{
  home.packages = [ ghcEnv ] ++ (with pkgs.haskellPackages; [
    brittany
    cabal-bounds
    cabal-fmt
    cabal-install
    ghcid
    ghcide
    hlint
    ormolu
    stack
  ]);

  programs.vscode = {
    extensions = with pkgs.vscode-extensions; [
      haskell.haskell
      justusadam.language-haskell
    ];
    userSettings.languageServerHaskell = {
      hieVariant = "haskell-language-server";
      formattingProvider = "ormolu";
    };
  };
}
