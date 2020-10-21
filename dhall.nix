{ pkgs, ... }: {
  home.packages = with pkgs; [ dhall dhall-json dhall-lsp-server ];

  programs.vscode = {
    extensions = with pkgs.vscode-extensions.dhall; [
      dhall-lang
      vscode-dhall-lsp-server
    ];
  };
}
