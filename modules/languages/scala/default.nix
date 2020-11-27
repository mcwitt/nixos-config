{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.scala;
in
{
  options.languages.scala.enable = mkEnableOption "Scala language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ metals sbt scalafmt ];

    programs.emacs.init.usePackage = {

      lsp-metals.enable = true;

      sbt-mode = {
        enable = true;
        command = [ "sbt-start" "sbt-command" ];
        config = ''
          ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
          ;; allows using SPACE when in the minibuffer
          (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map)
          ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
          (setq sbt:program-options '("-Dsbt.supershell=false"))
        '';
      };

      scala-mode = {
        enable = true;
        mode = [ ''"\\.s\\(cala\\|bt\\)$"'' ];
        hook = [ "(scala-mode . lsp)" ];
      };
    };

    programs.vscode = {
      extensions = with pkgs.vscode-extensions; [
        scala-lang.scala
        scalameta.metals
      ];
      userSettings.metals.javaHome = pkgs.jdk11_headless;
    };
  };
}
