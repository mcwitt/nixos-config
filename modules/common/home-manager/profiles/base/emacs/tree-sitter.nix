{
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
