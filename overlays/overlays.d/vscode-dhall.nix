self: super:
let
  inherit (super) stdenv;
  inherit (super.vscode-utils) buildVscodeMarketplaceExtension;
in {
  vscode-extensions = super.vscode-extensions // {
    dhall.dhall-lang = buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "dhall-lang";
        publisher = "dhall";
        version = "0.0.4";
        sha256 = "0sa04srhqmngmw71slnrapi2xay0arj42j4gkan8i11n7bfi1xpf";
      };
      meta = { license = stdenv.lib.licenses.mit; };
    };

    dhall.vscode-dhall-lsp-server = buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "vscode-dhall-lsp-server";
        publisher = "dhall";
        version = "0.0.4";
        sha256 = "1zin7s827bpf9yvzpxpr5n6mv0b5rhh3civsqzmj52mdq365d2js";
      };
      meta = { license = stdenv.lib.licenses.mit; };
    };
  };
}
