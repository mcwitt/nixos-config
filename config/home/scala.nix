{ pkgs, ... }: {
  home.packages = with pkgs; [ metals sbt scalafmt ];
  programs.vscode.extensions = with pkgs.vscode-extensions; [
    scala-lang.scala
    scalameta.metals
  ];
}
