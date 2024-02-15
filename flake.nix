{
  description = "NixOS configuration";

  inputs = {
    base16-rofi.flake = false;
    base16-rofi.url = "github:tinted-theming/base16-rofi";
    base16-tmux.flake = false;
    base16-tmux.url = "github:tinted-theming/base16-tmux";
    copilot-el.flake = false;
    copilot-el.url = "github:zerolfx/copilot.el";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    firefly.url = "github:timhae/firefly";
    fish-kubectl-completions.flake = false;
    fish-kubectl-completions.url = "github:evanlucas/fish-kubectl-completions";
    flake-utils.url = "github:numtide/flake-utils";
    "fzf.fish".flake = false;
    "fzf.fish".url = "github:PatrickF1/fzf.fish";
    git-sync-el.flake = false;
    git-sync-el.url = "github:mcwitt/git-sync.el";
    gitignore.flake = false;
    gitignore.url = "github:github/gitignore";
    home-assistant-eero.flake = false;
    home-assistant-eero.url = "github:schmittx/home-assistant-eero";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nur.url = "github:nix-community/NUR";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    stylix.url = "github:danth/stylix/release-23.11";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , home-manager
    , emacs-overlay
    , nur
    , pre-commit-hooks
    , stylix
    , ...
    } @ inputs:
    let
      overlays = [
        self.overlays.default
        emacs-overlay.overlay
        nur.overlay
      ];

      mkExtraSpecialArgs = pkgs: {
        inherit inputs;

        lib = nixpkgs.lib.extend (final: prev: import ./lib.nix { inherit inputs; } final prev // home-manager.lib);

        # gross hack to use modules from NUR
        # https://discourse.nixos.org/t/importing-nur-home-manager-modules-in-nix-flakes/16457
        nurNoPkgs = import nur {
          pkgs = null;
          nurpkgs = pkgs;
        };
      };
    in
    {
      overlays.default = import ./overlay { inherit inputs; };

      nixosConfigurations =
        let
          makeNixosSystem = nixpkgs.lib.makeOverridable ({ system, users, extraNixosModules, extraHmModules }:
            nixpkgs.lib.nixosSystem {
              inherit system;

              specialArgs = { inherit inputs; };

              modules = [
                home-manager.nixosModules.home-manager
                nur.nixosModules.nur

                stylix.nixosModules.stylix
                self.nixosModules.stylix

                self.nixosModules.common
                self.nixosModules.nixos

                ({ config, lib, pkgs, ... }: {
                  nixpkgs = {
                    config.allowUnfree = true;
                    inherit overlays;
                  };

                  users.users = builtins.listToAttrs (map
                    (user: lib.nameValuePair user {
                      isNormalUser = true;
                      extraGroups = [ "wheel" "docker" "video" ];
                      shell = pkgs.fish;
                    })
                    users);

                  home-manager = {
                    useGlobalPkgs = true;
                    useUserPackages = true;
                    extraSpecialArgs = mkExtraSpecialArgs pkgs;

                    users = builtins.listToAttrs (map
                      (user: lib.nameValuePair user {
                        imports = [
                          self.homeManagerModules.common
                          self.homeManagerModules.nixos
                        ] ++ extraHmModules;
                      })
                      users);
                  };

                  profiles.base.enable = true;
                })
              ] ++ extraNixosModules;
            });
        in
        {
          golem = makeNixosSystem {
            system = "x86_64-linux";
            users = [ "matt" ];
            extraNixosModules = [
              ./hosts/golem/configuration
              {
                home-manager.users.matt.profiles = {
                  base.enable = true;
                  desktop.enable = true;
                  personal.enable = true;
                };
                profiles.personal.enable = true;
              }
            ];
            extraHmModules = [ ./hosts/golem/home ];
          };

          hal = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
            system = "aarch64-linux";
            modules = [
              nur.nixosModules.nur
              self.nixosModules.common
              self.nixosModules.nixos
              ./hosts/hal/configuration
              {
                nixpkgs = {
                  config.allowUnfree = true;
                  inherit overlays;
                };
              }
            ];
            specialArgs = { inherit inputs; };
          };

          hestia = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
            system = "aarch64-linux";
            modules = [
              self.nixosModules.common
              self.nixosModules.nixos
              ./hosts/hestia/configuration
              {
                nixpkgs = {
                  config.allowUnfree = true;
                  inherit overlays;
                };
              }
            ];
            specialArgs = { inherit inputs; };
          };

          karakuri = makeNixosSystem {
            system = "x86_64-linux";
            users = [ "matt" ];
            extraNixosModules = [
              ./hosts/karakuri/configuration
              {
                home-manager.users.matt.profiles = {
                  base.enable = true;
                  desktop.enable = true;
                  personal.enable = true;
                };
                profiles.personal.enable = true;
              }
            ];
            extraHmModules = [ ./hosts/karakuri/home ];
          };
        };

      homeConfigurations =
        let
          pkgsFor = system: import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            inherit overlays;
          };
        in
        {
          matt = nixpkgs.lib.makeOverridable home-manager.lib.homeManagerConfiguration rec {

            pkgs = pkgsFor "x86_64-linux";

            extraSpecialArgs = mkExtraSpecialArgs pkgs;

            modules = [
              self.homeManagerModules.common
              self.homeManagerModules.nixos

              stylix.homeManagerModules.stylix
              self.homeManagerModules.stylix

              ({ config, lib, ... }: {
                home = {
                  username = lib.mkDefault "matt";
                  homeDirectory = "/home/${config.home.username}";
                  stateVersion = lib.mkDefault "22.11";
                };
              })
            ];
          };

          "matt@desktop" = self.homeConfigurations.matt.override (old: {
            modules = old.modules ++ [{
              profiles = {
                desktop.enable = true;
                personal.enable = true;
              };
            }];
          });

          "matt@macos" = nixpkgs.lib.makeOverridable home-manager.lib.homeManagerConfiguration rec {

            pkgs = pkgsFor "x86_64-darwin";

            extraSpecialArgs = mkExtraSpecialArgs pkgs;

            modules = [
              self.homeManagerModules.common

              stylix.homeManagerModules.stylix
              self.homeManagerModules.stylix

              ({ config, lib, ... }: {
                home = {
                  username = lib.mkDefault "matt";
                  homeDirectory = "/Users/${config.home.username}";
                  stateVersion = lib.mkDefault "22.11";
                };

                programs.wezterm.enable = lib.mkForce false; # TODO: broken on x86_64-darwin

                stylix.targets.swaylock.enable = false; # swaylock unsupported on x86_64-darwin; unclear why this is needed
              })
            ];
          };
        };

      nixosModules = {
        common = import ./modules/common/nixos;
        nixos = import ./modules/nixos/nixos;
        stylix = import ./modules/stylix;
      };

      homeManagerModules = {
        common = import ./modules/common/home-manager;
        nixos = import ./modules/nixos/home-manager;
        darwin = import ./modules/darwin/home-manager;
        stylix = import ./modules/stylix;
      };

    } // flake-utils.lib.eachDefaultSystem (system: {

      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          excludes = [ "hardware-configuration\\.nix" ];
          hooks = {
            nixpkgs-fmt.enable = true;
          };
        };
      };

      devShells.default = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
      };
    });
}
