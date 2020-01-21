# Nix configuration files

Personal Nix configurations and modules, inspired by [jwiegly's configuration][jwiegley-nix-config].

## Config files

- `configuration.nix`: [NixOS][nixos] configuration. Symlinked into `/etc/nixos/`.
- `darwin-configuration.nix`: [nix-darwin][] configuration. This is
  similar to `configuration.nix` on NixOS and provides declarative
  configuration for macOS. Symlinked into `~/.config/nixpkgs/`.
- `home.nix`: [home-manager][] configuration. This is used for
  declarative specification of user-specific configuration
  (e.g. "dotfiles"). Symlinked into `~/.config/nixpkgs`.

## Modules

The following modules are imported by the above configurations. The
main motivation is to declare a shared base environment for both NixOS
and macOS machines.

- `packages.nix`
- `fonts.nix`

[jwiegley-nix-config]: https://github.com/jwiegley/nix-config
[nixos]: https://nixos.org
[nix-darwin]: https://github.com/LnL7/nix-darwin
[home-manager]: https://github.com/rycee/home-manager
