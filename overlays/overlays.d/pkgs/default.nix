self: super:
let inherit (self) callPackage;
in
{
  mcwitt = {
    gitignore = callPackage ./gitignore.nix { };
    lib = callPackage ./lib.nix { };
    scripts = callPackage ./scripts { };
    vscode-extensions = callPackage ./vscode-extensions { };
  };
}
