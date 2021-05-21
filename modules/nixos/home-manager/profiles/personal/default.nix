{ pkgs, ... }:
{
  home.packages = with pkgs; [
    anki
    discord
    element-desktop
    gimp
    peek
    slack
    steam
    zulip
  ];
}
