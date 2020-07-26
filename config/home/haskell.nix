# Global Haskell environment for experimentation and ad-hoc usage
# (usually overridden by project-specific environment)
{ pkgs, ... }:
let
  ghcEnv = pkgs.haskellPackages.ghcWithPackages (ps:
    with ps; [
      array
      checkers
      containers
      fgl
      heaps
      hspec
      lens
      monad-loops
      mtl
      parsec
      protolude
      split
      transformers
      vector
    ]);

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
