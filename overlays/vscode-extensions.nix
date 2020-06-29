self: super:
let
  inherit (self) stdenv;
  inherit (self.vscode-utils) buildVscodeMarketplaceExtension;
in {
  vscode-extensions = super.vscode-extensions // {
    alanz.vscode-hie-server = buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "vscode-hie-server";
        publisher = "alanz";
        version = "0.0.40";
        sha256 = "1cmlgidjma41s5zq5161gcxxmk5lfzcm8dvznls04y5l7q9b0gca";
      };
      meta = { license = stdenv.lib.licenses.mit; };
    };
  };
}
