{ pkgs, ... }:
{
  home.packages = with pkgs; [
    anki
    discord
    slack
    steam
    zulip
  ];
}
