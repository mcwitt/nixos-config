self: super:
let
  inherit (super.vscode-utils) buildVscodeMarketplaceExtension;
  lib = super.lib;
in
{
  vscode-extensions = super.vscode-extensions // {
    elmtooling.elm-ls-vscode = buildVscodeMarketplaceExtension {
      meta = with lib; {
        changelog = "https://marketplace.visualstudio.com/items/Elmtooling.elm-ls-vscode/changelog";
        description = "Elm language server";
        downloadPage = "https://marketplace.visualstudio.com/items?itemName=Elmtooling.elm-ls-vscode";
        homepage = "https://github.com/elm-tooling/elm-language-client-vscode";
        license = licenses.mit;
        maintainers = with maintainers; [ mcwitt ];
      };
      mktplcRef = {
        name = "elm-ls-vscode";
        publisher = "Elmtooling";
        version = "2.0.1";
        sha256 = "06x5ld2r1hzns2s052mvhmfiaawjzcn0jf5lkfprhmrkxnmfdd43";
      };
    };
  };
}
