self: super:
let inherit (self.vscode-utils) buildVscodeMarketplaceExtension;
in {
  vscode-extensions = super.vscode-extensions // {
    DigitalAssetHoldingsLLC.ghcide = buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "ghcide";
        publisher = "DigitalAssetHoldingsLLC";
        version = "0.0.2";
        sha256 = "02gla0g11qcgd6sjvkiazzk3fq104b38skqrs6hvxcv2fzvm9zwf";
      };
      meta = { license = self.stdenv.lib.licenses.asl20; };
    };
  };
}
