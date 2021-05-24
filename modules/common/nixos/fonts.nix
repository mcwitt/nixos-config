{ pkgs, ... }:
{
  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
    fira-code
    fira-code-symbols
    iosevka
    joypixels
    nerdfonts
    source-code-pro
  ];

  nixpkgs.config.joypixels.acceptLicense = true;
}
