{ pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
    fira-code
    fira-code-symbols
    source-code-pro
  ];
}
