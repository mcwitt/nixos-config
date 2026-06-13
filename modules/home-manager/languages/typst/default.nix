{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.typst;
in
{
  options.languages.typst.enable = mkEnableOption "Typst language environment";

  config = mkIf cfg.enable {

    home.packages = [
      pkgs.tinymist
      pkgs.typstyle
    ];

    # typst-ts-mode puts an ;;;###autoload cookie on its
    # `define-compilation-mode' form (typst-ts-compile.el), which copies the
    # macro call verbatim into the generated package autoloads (and our
    # package-quickstart). Loading those at startup fails with
    # (void-function define-compilation-mode) because compile.el isn't loaded
    # during package activation, aborting the rest of the autoloads file.
    # The cookie is redundant: typst-ts-compile.el itself `(require 'compile)`s
    # before defining the mode, so stripping it loses nothing. elpaBuild reads
    # the ELPA .tar directly (no unpack phase), so we patch the tarball rather
    # than via postPatch. Still broken on upstream main as of 2026-06-12.
    # TODO: drop once fixed upstream (https://codeberg.org/meow_king/typst-ts-mode).
    programs.emacs.overrides = _: super: {
      typst-ts-mode = super.typst-ts-mode.overrideAttrs (old: {
        src = pkgs.runCommand "typst-ts-mode-patched-src.tar" { } ''
          mkdir unpacked
          tar xf ${old.src} -C unpacked
          sed -i -z \
            's/;;;###autoload\n(define-compilation-mode/(define-compilation-mode/' \
            unpacked/*/typst-ts-compile.el
          tar cf "$out" -C unpacked "$(ls unpacked)"
        '';
      });
    };

    programs.emacs.init.usePackage = {
      typst-ts-mode.enable = true;

      eglot = {
        enable = true;
        hook = [ "(typst-ts-mode . eglot-ensure)" ];
        config = ''
          (add-to-list 'eglot-server-programs
                       '(typst-ts-mode . ("tinymist" "lsp")))
        '';
      };

      reformatter = {
        enable = true;
        command = [ "typstyle-on-save-mode" ];
        config = ''
          (reformatter-define typstyle
            :program "typstyle"
            :args '("--line-width" "80" "--wrap-text")
            :lighter " Typstyle")
        '';
      };
    };
  };
}
