{ emacs, emacsPackagesGen, mypkgs }:
(emacsPackagesGen emacs).emacsWithPackages mypkgs.emacsPackages
