{ pkgs, ... }: { home.packages = with pkgs; [ metals sbt scalafmt ]; }
