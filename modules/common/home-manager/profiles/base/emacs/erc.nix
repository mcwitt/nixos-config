{
  programs.emacs.init.usePackage = {

    erc = {
      enable = true;
      command = [ "erc" "my/erc-libera-chat" ];
      config = ''
        (setq erc-prompt-for-password nil) ; get login from ~/.authinfo.gpg
        (setq erc-hide-list '("JOIN" "PART" "QUIT"))

        (add-to-list 'erc-modules 'autojoin)
        (add-to-list 'erc-modules 'notifications)
        (add-to-list 'erc-modules 'spelling)
        (erc-update-modules)

        (setq erc-autojoin-channels-alist
              '(("#emacs"
                 "#haskell"
                 "#nixos")))
        (setq erc-autojoin-timing 'ident)

        (defun my/erc-libera-chat ()
          "Connect to Libera Chat with ERC."
          (interactive)
          (erc :server "irc.libera.chat" :port 6667 :nick "mcwitt"))
      '';
    };

    erc-hl-nicks = {
      enable = true;
      after = [ "erc" ];
    };

    erc-image = {
      enable = true;
      after = [ "erc" ];
    };
  };
}
