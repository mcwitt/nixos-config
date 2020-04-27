# nix-config

Personal Nix configuration modules and derivations. Inspired by [jwiegley's configuration][jwiegley-nix-config].

## Configuration modules

- **`config/configuration.nix`**: [NixOS][nixos] configuration. Typically symlinked into `/etc/nixos/`.

- **`config/darwin-configuration.nix`**: [nix-darwin][] configuration. This is similar to `configuration.nix` on NixOS and provides declarative configuration for darwin. Typically symlinked into `~/.config/nixpkgs/`.

- **`config/home/home.nix`**: [home-manager][] configuration. This is for user-specific configuration and dotfiles. Typically symlinked into `~/.config/nixpkgs`.

`config/` also contains shared modules that are imported into multiple configurations (e.g. `config/packages.nix`).

## Custom derivations

Custom derivations (packages) live in `pkgs`. Definitions here should have a corresponding entry in `pkgs/default.nix`. These derivations are available in the configuration via the path `pkgs.mypkgs` (via the `overlays/my-packages.nix` overlay).

## Overlays

[Overlays][overlays] extend the [nixpkgs][] repository, adding new packages or modifying existing ones. For example, `overlays/my-packages.nix` makes it possible to reference custom derivations from configuration modules and other Nix tools (as `pkgs.mypkgs.<name>`).

[jwiegley-nix-config]: https://github.com/jwiegley/nix-config
[nixos]: https://nixos.org
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nix-darwin]: https://github.com/LnL7/nix-darwin
[home-manager]: https://github.com/rycee/home-manager
[overlays]: https://nixos.org/nixpkgs/manual/#chap-overlays
