{ config, lib, pkgs, ... }:
let
  iosevka-ligatures-elisp = builtins.readFile (
    let configGen = pkgs.fetchurl {
      url = https://gist.githubusercontent.com/mrkgnao/49c7480e1df42405a36b7ab09fe87f3d/raw/fa08b43e29aea1275b994bf8dd431ea69609338e/IosevkaConfigGen.hs;
      sha256 = "sha256-C5GX5aPLOHW6Bot2lrZuL66rZc6duuumUSBjbAQDz4Q=";
    }; in
    pkgs.runCommand "iosevka-ligatures-elisp" { buildInputs = [ pkgs.haskellPackages.ghc ]; } ''
      ghci -v0 ${configGen} <<< toElisp > $out
    ''
  );
in
{
  options.programs.emacs.init.faces.height = with lib; mkOption {
    type = types.ints.positive;
    default = 100;
  };

  config.programs.emacs.init = {
    earlyInit = ''
      (set-face-attribute 'default
                          nil
                          :height ${builtins.toString config.programs.emacs.init.faces.height}
                          :family "Iosevka Custom")
      (set-face-attribute 'variable-pitch
                          nil
                          :family "DejaVu Sans")
    '';

    usePackage.iosevka-mode = {
      enable = true;
      package = epkgs:
        epkgs.trivialBuild {
          pname = "iosevka-mode";
          version = "2022-06-06";
          src = pkgs.writeText "iosevka-mode.el" ''
            ;; Adapted from https://github.com/jming422/fira-code-mode/blob/master/fira-code-mode.el
            (defvar-local iosevka-mode--enabled-prettify-mode nil)
            (defvar iosevka-mode--old-prettify-alist)

            (defun iosevka-mode--enable ()
              "Enable Iosevka ligatures in current buffer."
              (setq-local iosevka-mode--old-prettify-alist prettify-symbols-alist)
              (let ((new-prettify-alist (append '(${iosevka-ligatures-elisp}) iosevka-mode--old-prettify-alist)))
                (setq-local prettify-symbols-alist new-prettify-alist))
              (unless prettify-symbols-mode
                (prettify-symbols-mode t)
                (setq-local iosevka-mode--enabled-prettify-mode t)))

            (defun iosevka-mode--disable ()
              "Disable Iosevka ligatures in current buffer."
              (setq-local prettify-symbols-alist iosevka-mode--old-prettify-alist)
              (when iosevka-mode--enabled-prettify-mode
                (prettify-symbols-mode -1)
                (setq-local iosevka-mode--enabled-prettify-mode nil)))

            ;;;###autoload
            (define-minor-mode iosevka-mode
              "Iosevka ligatures minor mode"
              :lighter "  \xe15b"
              :group 'iosevka-ligatures
              (unless (display-graphic-p)
                (display-warning '(iosevka-ligatures) "iosevka-mode probably won't work for non-graphical displays!"))
              (setq-local prettify-symbols-unprettify-at-point 'right-edge)
              (if iosevka-mode
                  (iosevka-mode--enable)
                (iosevka-mode--disable)))

            ;;;###autoload
            (define-globalized-minor-mode global-iosevka-mode iosevka-mode
              iosevka-mode)
          '';
        };
    };
  };
}
