{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.typescript;
in
{
  options.languages.typescript.enable = mkEnableOption "TypeScript language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      biome
      nodejs
      pnpm
      typescript
      vtsls
    ];

    programs.emacs.init.usePackage = {

      eglot = {
        hook = [
          "(typescript-ts-mode . eglot-ensure)"
          "(tsx-ts-mode . eglot-ensure)"
        ];
        config = ''
          (add-to-list 'eglot-server-programs
                       '((typescript-ts-mode tsx-ts-mode) . ("vtsls" "--stdio")))
        '';
      };

      format-all.hook = [
        "(typescript-ts-mode . (lambda () (format-all-mode -1)))"
        "(tsx-ts-mode . (lambda () (format-all-mode -1)))"
      ];

      ligature.config = ''
        (ligature-set-ligatures '(typescript-ts-mode tsx-ts-mode)
                                '("->" "=>" "==" "===" "!=" "!==" ">=" "<=" "&&" "||" "??" "?."))
      '';

      reformatter = {
        enable = true;
        command = [
          "biome-format-on-save-mode"
          "biome-fix-on-save-mode"
        ];
        config = ''
          (reformatter-define biome-format
            :program "biome"
            :args (list "format" "--stdin-file-path" (or (buffer-file-name) input-file))
            :lighter " BiomeFmt")

          (reformatter-define biome-fix
            :program "biome"
            :args (list "check" "--write" "--stdin-file-path" (or (buffer-file-name) input-file))
            :lighter " BiomeFix")
        '';
      };

      subword.hook = [
        "(typescript-ts-mode . subword-mode)"
        "(tsx-ts-mode . subword-mode)"
      ];

      typescript-ts-mode = {
        enable = true;
        mode = [
          ''("\\.ts\\'" . typescript-ts-mode)''
          ''("\\.mts\\'" . typescript-ts-mode)''
          ''("\\.cts\\'" . typescript-ts-mode)''
        ];
      };

      tsx-ts-mode = {
        enable = true;
        mode = [ ''("\\.tsx\\'" . tsx-ts-mode)'' ];
      };
    };

    programs.git.ignores = pkgs.gitignores "Node";

    programs.vscode.profiles.default = mkIf (!pkgs.stdenv.isDarwin) {
      extensions = with pkgs.vscode-extensions; [
        biomejs.biome
      ];
    };
  };
}
