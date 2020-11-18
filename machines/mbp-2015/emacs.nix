{ mypkgs }:
{
  home.file.".emacs.d" = {
    source = "${mypkgs.dotfiles}/emacs.d/";
    recursive = true;
  };
}
