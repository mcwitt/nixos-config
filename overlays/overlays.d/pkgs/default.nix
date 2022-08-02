self: _:
{
  mcwitt = {
    gitignore = self.callPackage ./gitignore.nix { };
    lib = self.callPackage ./lib.nix { };
    vscode-extensions = self.callPackage ./vscode-extensions { };
  };
}
