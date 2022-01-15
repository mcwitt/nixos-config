{ pkgs, ... }:
{
  programs.emacs.overrides =
    let nixpkgs = import
      (pkgs.fetchFromGitHub {
        owner = "mcwitt";
        repo = "nixpkgs";
        rev = "884c2eed250306bc7523448b2b93613bbd3f1324";
        sha256 = "sha256-GckygchPdeYwau6Gze7uncN5cIpkwkQdllbAcdKtrJE=";
      })
      { }; in
    _: _: {
      inherit (nixpkgs.emacsPackages) tsc tree-sitter-langs;
    };

  programs.emacs.init.usePackage = {

    tree-sitter = {
      enable = true;
      config = ''
        (global-tree-sitter-mode)
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
      '';
    };

    tree-sitter-langs.enable = true;
  };
}
