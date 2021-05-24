{
  programs.emacs.init = {
    earlyInit = ''
      (defun emoji-set-font (frame)
        "Adjust the font settings of FRAME so Emacs can display emoji properly."
        (if (eq system-type 'darwin)
            ;; For NS/Cocoa
            (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
          ;; For Linux
          (set-fontset-font t 'symbol (font-spec :family "JoyPixels") frame 'prepend)))
      (emoji-set-font nil)
      (add-hook 'after-make-frame-functions 'emoji-set-font)
    '';

    usePackage.company-emoji = {
      enable = true;
      config = ''
        (add-to-list 'company-backends 'company-emoji)
      '';
    };
  };
}
