# Nix configuration files

Personal Nix configurations and modules, heavily inspired by
[jwiegley's configuration][jwiegley-nix-config].

## Config files

- `config/configuration.nix`: [NixOS][nixos] configuration. Typically
  symlinked into `/etc/nixos/`.
- `config/darwin-configuration.nix`: [nix-darwin][]
  configuration. This is similar to `configuration.nix` on NixOS and
  provides declarative configuration for macOS. Typically symlinked
  into `~/.config/nixpkgs/`.
- `config/home/home.nix`: [home-manager][] configuration. This is for
  user-specific configuration (e.g. "dotfiles"). Typically symlinked
  into `~/.config/nixpkgs`.

`config/` also contains shared modules that are imported into multiple
configurations (e.g. `config/packages.nix`).

## Overlays

[Overlays][overlays] extend the [nixpkgs][] repository, adding new
packages or modifying existing ones. For example,
`overlays/dotfiles.nix` packages my personal dotfiles repository as
`mcwitt-dotfiles`, making it available config modules:

``` nix
{ pkgs, ... }: {

  # ...

  home.file.".emacs.d" = {
    source = "${pkgs.mcwitt-dotfiles}/emacs.d/";
    recursive = true;
  };

  # ...

}
```

[jwiegley-nix-config]: https://github.com/jwiegley/nix-config
[nixos]: https://nixos.org
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nix-darwin]: https://github.com/LnL7/nix-darwin
[home-manager]: https://github.com/rycee/home-manager
[overlays]: https://nixos.org/nixpkgs/manual/#chap-overlays
