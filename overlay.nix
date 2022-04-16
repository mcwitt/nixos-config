_: super:
let inherit (super) lib; in {

  vscode-extensions = super.vscode-extensions // {
    NVIDIA.nsight-vscode-edition = super.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "nsight-vscode-edition";
        publisher = "NVIDIA";
        version = "2021.1.30130113";
        sha256 = "sha256-vpM0T4UGSasBNtk53//vqXUTSCVt1JsFCHPloI53U0Q=";
      };
      meta = { license = super.lib.licenses.unfree; };
    };
  };
}
