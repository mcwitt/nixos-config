# nix-config

Personal NixOS configurations and nixpkgs overlays.

## Setup instructions

1. Clone this repo

    ``` sh
    git clone git@github.com:mcwitt/nix-config.git
    cd nix-config
    ```

2. If not on NixOS, skip to [General](#general) below.

### NixOS

1. Symlink [configuration.nix](configuration.nix) into `/etc/nixos/`

    ``` sh
    ln -s $PWD/configuration.nix /etc/nixos/
    ```

2. Build and switch to the NixOS configuration

    ``` sh
    sudo nixos-rebuild switch
    ```


### <a name="general"></a>General

1. If not on NixOS, ensure that the [Nix][nixos] package manager is installed.

2. Ensure [home-manager][] is installed.

3. Symlink desired configuration files into `~/.config/nixpkgs`

    ``` sh
    mkdir -p ~/.config/nixpkgs
    ln -s $PWD/config.nix ~/config/nixpkgs/                                  # nixpkgs config
    ln -s $PWD/overlays/overlays.d ~/config/nixpkgs/overlays                 # nixpkgs overlays
    ln -s $PWD/machines/mbp-2015/home.nix ~/config/nixpkgs/                  # home-manager config
    ln -s $PWD/machines/mbp-2015/darwin-configuration.nix ~/config/nixpkgs/  # nix-darwin config (only on macOS)
    ```

4. Build and switch to the home-manager configuration (and nix-darwin configuration, if on macOS)

    ``` sh
    home-manager switch
    darwin-rebuild switch  # only on macOS
    ```


## Description of contents

### Configuration modules

- **`configuration.nix`**: [NixOS][nixos] configuration. Typically symlinked into `/etc/nixos/`.

- **`darwin-configuration.nix`**: [nix-darwin][] configuration. This is similar to `configuration.nix` on NixOS and provides declarative configuration for darwin. Typically symlinked into `~/.config/nixpkgs/`.

- **`home.nix`**: [home-manager][] configuration. This is for user-specific configuration and dotfiles. Typically symlinked into `~/.config/nixpkgs`.

### Overlays

[Overlays][overlays] extend the [nixpkgs][] repository, adding new packages or modifying existing ones. For example, `overlays/overlays.d/pkgs` adds custom derivations to the path `pkgs.mypkgs.<name>`.

### Custom derivations

Custom derivations are kept in [`overlays/overlays.d/pkgs`](`overlays/overlays.d/pkgs`). Each derivation should have a corresponding entry in [`overlays/overlays.d/pkgs/default.nix`](overlays/overlays.d/pkgs/default.nix); this ensures that it will be available from configuration modules via the attribute path `pkgs.mypkgs`.

[jwiegley-nix-config]: https://github.com/jwiegley/nix-config
[nixos]: https://nixos.org
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nix-darwin]: https://github.com/LnL7/nix-darwin
[home-manager]: https://github.com/rycee/home-manager
[overlays]: https://nixos.org/nixpkgs/manual/#chap-overlays
