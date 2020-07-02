# nix-config

Personal NixOS configurations and nixpkgs overlays.

## Setup instructions

### General

1. If you are not on NixOS, ensure that the [Nix][nixos] package manager is installed.
2. Clone this repo locally

    ``` sh
    git clone git@github.com:mcwitt/nix-config.git
    ```

3. Ensure [home-manager][] is installed.
4. Symlink [config/home/home.nix](/config/home/home.nix) into `~/.config/nixpkgs/`

    ``` sh
    ln -s $(pwd)/nix-config/config/home/home.nix ~/.config/nixpkgs/home.nix
    ```

5. Build and switch to the home-manager configuration

    ``` sh
    home-manager switch
    ```

6. Follow OS-specific instructions below

### NixOS

1. Symlink [config/configuration.nix](/config/configuration.nix) into `/etc/nixos/`

    ``` sh
    ln -s $(pwd)/nix-config/config/configuration.nix /etc/nixos/configuration.nix
    ```

2. Build and switch to the NixOS configuration

    ``` sh
    sudo nixos-rebuild switch
    ```

### macOS

1. Ensure [nix-darwin][] is installed.
2. Symlink [config/darwin-configuration.nix](/config/darwin-configuration.nix) into `~/.config/nixpkgs/`

    ``` sh
    ln -s $(pwd)/nix-config/config/darwin-configuration.nix ~/.config/nixpkgs/darwin-configuration.nix
    ```

3. Build and switch to the nix-darwin configuration

    ``` sh
    darwin-rebuild switch
    ```


## Description of contents

### Configuration modules

- **`config/configuration.nix`**: [NixOS][nixos] configuration. Typically symlinked into `/etc/nixos/`.

- **`config/darwin-configuration.nix`**: [nix-darwin][] configuration. This is similar to `configuration.nix` on NixOS and provides declarative configuration for darwin. Typically symlinked into `~/.config/nixpkgs/`.

- **`config/home/home.nix`**: [home-manager][] configuration. This is for user-specific configuration and dotfiles. Typically symlinked into `~/.config/nixpkgs`.

`config/` also contains shared modules that are imported into multiple configurations (e.g. `config/packages.nix`).

### Overlays

[Overlays][overlays] extend the [nixpkgs][] repository, adding new packages or modifying existing ones. For example, `overlays/pkgs` adds custom derivations to the path `pkgs.mypkgs.<name>`.

### Custom derivations

Custom derivations are kept in `overlays/pkgs`. Each derivation should have a corresponding entry in `overlays/pkgs/default.nix`; this ensures that it will be available from configuration modules via the path `pkgs.mypkgs`.

[jwiegley-nix-config]: https://github.com/jwiegley/nix-config
[nixos]: https://nixos.org
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nix-darwin]: https://github.com/LnL7/nix-darwin
[home-manager]: https://github.com/rycee/home-manager
[overlays]: https://nixos.org/nixpkgs/manual/#chap-overlays
