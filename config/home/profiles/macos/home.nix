{ pkgs, lib, ... }: {
  imports = [ ../../home.nix ];

  programs.git.ignores = pkgs.mypkgs.gitignore.ghGitIgnoreLines "Global/macOS";

  programs.password-store.package =
    pkgs.pass.withExtensions (exts: [ exts.pass-update ]);
}
