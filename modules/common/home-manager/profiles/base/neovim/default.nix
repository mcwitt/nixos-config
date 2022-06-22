{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    extraPackages = [ pkgs.nodejs-16_x ];
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      YouCompleteMe
      (copilot-vim.overrideAttrs (_: {
        version = "1.4.2";
        src = pkgs.fetchFromGitHub {
          owner = "github";
          repo = "copilot.vim";
          rev = "c2e75a3a7519c126c6fdb35984976df9ae13f564";
          sha256 = "sha256-V13La54aIb3hQNDE7BmOIIZWy7In5cG6kE0fti/wxVQ=";
        };
      }))
      ctrlp-vim
      nvim-lightbulb
      syntastic
      tagbar
      vim-airline
      vim-fugitive
      vim-gitgutter
      vim-surround
    ] ++ (
      let nvim-treesitter = pkgs.vimPlugins.nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars);
      in [ nvim-treesitter ]
    )
    ;
    extraConfig = ''
      set expandtab
      set shiftwidth=2
      set softtabstop=2
      set nojoinspaces

      " fd returns to normal mode
      inoremap fd <esc>

      set statusline+=%#warningmsg#
      set statusline+=%{SyntasticStatuslineFlag()}
      set statusline+=%*

      let g:syntastic_always_populate_loc_list = 1
      let g:syntastic_auto_loc_list = 1
      let g:syntastic_check_on_open = 1
      let g:syntastic_check_on_wq = 0
    '';
  };
}
