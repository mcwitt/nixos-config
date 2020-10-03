# nix-config

Personal NixOS configurations and nixpkgs overlays.

## Setup instructions

### General

1. If you are not on NixOS, ensure that the [Nix][nixos] package manager is installed.
2. Clone this repo into `~/.config/nixpkgs`

    ``` sh
    git clone git@github.com:mcwitt/nix-config.git ~/.config/nixpkgs
    ```

3. Ensure [home-manager][] is installed.

4. Symlink the desired home-manager configuration (and nix-darwin configuration, if on macOS)
    ``` sh
    cd ~/.config/nixpkgs
    ln -s machines/mbp-2015/home.nix .
    ln -s machines/mbp-2015/darwin-configuration.nix .  # only on macOS
    ```

4. Build and switch to the home-manager configuration (and nix-darwin configuration, if on macOS)

    ``` sh
    home-manager switch
    darwin-rebuild switch  # only on macOS
    ```

5. Follow OS-specific instructions below

### NixOS

1. Symlink [config/configuration.nix](/config/configuration.nix) into `/etc/nixos/`

    ``` sh
    ln -s ~/.config/nixpkgs/nixos/configuration.nix /etc/nixos/
    ```

2. Build and switch to the NixOS configuration

    ``` sh
    sudo nixos-rebuild switch
    ```


## Description of contents

### Configuration modules

- **`configuration.nix`**: [NixOS][nixos] configuration. Typically symlinked into `/etc/nixos/`.

- **`darwin-configuration.nix`**: [nix-darwin][] configuration. This is similar to `configuration.nix` on NixOS and provides declarative configuration for darwin. Typically symlinked into `~/.config/nixpkgs/`.

- **`home.nix`**: [home-manager][] configuration. This is for user-specific configuration and dotfiles. Typically symlinked into `~/.config/nixpkgs`.

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
