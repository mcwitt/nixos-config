{ pkgs, ... }:

let
  ghcEnv =
    pkgs.haskell.packages.ghc883.ghcWithHoogle pkgs.mypkgs.myHaskellPackages;

in {
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
      alanz.vscode-hie-server
      justusadam.language-haskell
    ];
    userSettings.languageServerHaskell = {
      hieVariant = "ghcide";
      formattingProvider = "ormolu";
    };
  };
}
