{ lib, vscode-utils }:
let inherit (vscode-utils) buildVscodeMarketplaceExtension; in
{
  NVIDIA.nsight-vscode-edition = buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "nsight-vscode-edition";
      publisher = "NVIDIA";
      version = "2021.1.30130113";
      sha256 = "sha256-vpM0T4UGSasBNtk53//vqXUTSCVt1JsFCHPloI53U0Q=";
    };
    meta = { license = lib.licenses.unfree; };
  };
}
