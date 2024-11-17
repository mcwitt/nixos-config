{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    coc.enable = true;
    extraPackages = [ pkgs.nodejs ];
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      copilot-vim
      ctrlp-vim
      nvim-lightbulb
      syntastic
      tagbar
      vim-airline
      vim-autoformat
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

      au BufWrite * :Autoformat
    '';
  };
}
