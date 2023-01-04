{ inputs }:
final: prev:
{
  tree-sitter-grammars = prev.tree-sitter-grammars // {
    tree-sitter-python = prev.tree-sitter-grammars.tree-sitter-python.overrideAttrs (_: {
      nativeBuildInputs = [ final.nodejs final.tree-sitter ];
      configurePhase = ''
        tree-sitter generate --abi 13 src/grammar.json
      '';
    });
  };

  vscode-extensions = prev.vscode-extensions // {
    NVIDIA.nsight-vscode-edition = final.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "nsight-vscode-edition";
        publisher = "NVIDIA";
        version = "2021.1.30130113";
        sha256 = "sha256-vpM0T4UGSasBNtk53//vqXUTSCVt1JsFCHPloI53U0Q=";
      };
      meta = { license = final.lib.licenses.unfree; };
    };
  };

  lib = prev.lib.extend
    (final: _: {
      gitignores = path:
        final.splitString "\n"
          (builtins.readFile "${inputs.gitignore}/${path}.gitignore");

      setAll = value: keys: builtins.listToAttrs
        (map
          (key: final.nameValuePair key value)
          keys);
    });
}
