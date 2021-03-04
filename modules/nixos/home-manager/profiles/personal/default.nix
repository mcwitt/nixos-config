{ pkgs, ... }:
{
  home.packages = with pkgs; [
    anki
    discord
    gimp
    peek
    slack
    steam
    zulip
  ];
}
