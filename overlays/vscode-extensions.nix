self: super:
let
  inherit (self) stdenv;
  inherit (self.vscode-utils) buildVscodeMarketplaceExtension;
in {
  vscode-extensions = super.vscode-extensions // {
    haskell.haskell = buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "haskell";
        publisher = "haskell";
        version = "1.1.0";
        sha256 = "1wg06lyk0qn9jd6gi007sg7v0z9z8gwq7x2449d4ihs9n3w5l0gb";
      };
      meta = with stdenv.lib; { license = licenses.mit; };
    };
  };
}
