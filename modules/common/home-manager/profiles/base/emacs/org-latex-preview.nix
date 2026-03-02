{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    # use org-mode fork with upgraded org-latex-preview
    # https://code.tecosaur.net/tec/org-mode
    # TODO: move to specialization, or remove once changes are merged to org-mode

    programs.emacs = {
      overrides = self: _: {
        org =
          let
            rev = "1ef59f0aa02e3cff40bae68b756a29bc2001739e";
          in
          self.trivialBuild {
            pname = "org";
            version = "9.8pre+${builtins.substring 0 7 rev}";

            src =
              let
                root = pkgs.fetchgit {
                  name = "org-src";
                  url = "https://code.tecosaur.net/tec/org-mode.git";
                  inherit rev;
                  hash = "sha256-kPVFT2fgoOO5aCCAifzlSwDhMR2RqhhT1akOKfRyalw=";
                };
              in
              "${root}/lisp";
          };
      };

      init.usePackage.org = {
        hook = [ "(org-mode . org-latex-preview-auto-mode)" ];
        config = ''
          (setopt org-latex-preview-live t
                  org-latex-preview-live-debounce 0.25)

          ;; Increase size of latex fragment previews
          (plist-put org-latex-preview-appearance-options :page-width 0.8)
          (plist-put org-latex-preview-appearance-options :scale 1.35)
        '';
      };
    };
  };
}
