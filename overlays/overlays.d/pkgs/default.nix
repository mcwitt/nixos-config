self: _:
{
  mcwitt = {
    gitignore = self.callPackage ./gitignore.nix { };
    lib = self.callPackage ./lib.nix { };
    scripts = self.callPackage ./scripts { };
    vscode-extensions = self.callPackage ./vscode-extensions { };
  };
}
