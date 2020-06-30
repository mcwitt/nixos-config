{ emacsGit, emacsPackagesGen, mypkgs }:
(emacsPackagesGen emacsGit).emacsWithPackages mypkgs.emacsPackages
