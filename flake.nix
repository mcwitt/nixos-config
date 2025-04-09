{
  description = "NixOS configuration";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    stylix.url = "github:danth/stylix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      home-manager,
      emacs-overlay,
      nur,
      pre-commit-hooks,
      stylix,
      ...
    }@inputs:
    let
      overlays = [
        self.overlays.default
        emacs-overlay.overlay
        nur.overlays.default
      ];

      nixpkgsArgs = {
        inherit overlays;
        config.allowUnfree = true;
      };

      mkPkgs =
        {
          system,
          nixpkgs ? nixpkgs,
        }:
        import nixpkgs ({ inherit system; } // nixpkgsArgs);

      mkExtraSpecialArgs = pkgs: {
        inherit inputs;

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

      lib.makeNixosSystem = nixpkgs.lib.makeOverridable (
        {
          system,
          users,
          extraNixosModules ? [ ],
          extraHmModules ? [ ],
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;

          specialArgs = {
            inherit inputs;
          };

          modules = [
            home-manager.nixosModules.home-manager
            nur.modules.nixos.default

            stylix.nixosModules.stylix
            self.nixosModules.stylix

            self.nixosModules.common
            self.nixosModules.nixos

            (
              {
                config,
                lib,
                pkgs,
                ...
              }:
              {
                nixpkgs = nixpkgsArgs;

                users.users = builtins.listToAttrs (
                  map (
                    user:
                    lib.nameValuePair user {
                      isNormalUser = true;
                      extraGroups = [
                        "wheel"
                        "docker"
                        "video"
                      ];
                      shell = pkgs.fish;
                    }
                  ) users
                );

                home-manager = {
                  backupFileExtension = "backup-before-home-manager";

                  extraSpecialArgs = mkExtraSpecialArgs pkgs;

                  useGlobalPkgs = true;

                  users = builtins.listToAttrs (
                    map (
                      user:
                      lib.nameValuePair user {
                        imports = [
                          self.homeManagerModules.common
                          self.homeManagerModules.nixos
                        ] ++ extraHmModules;

                        profiles.base.enable = true;
                      }
                    ) users
                  );

                  useUserPackages = true;
                };

                profiles.base.enable = true;
              }
            )
          ] ++ extraNixosModules;
        }
      );

      nixosConfigurations = {
        golem = self.lib.makeNixosSystem {
          system = "x86_64-linux";
          users = [ "matt" ];
          extraNixosModules = [
            ./hosts/golem/configuration
            {
              profiles = {
                desktop.enable = true;
                personal.enable = true;
              };

              home-manager.users.matt.profiles = {
                desktop.enable = true;
                personal.enable = true;
              };
            }
          ];
          extraHmModules = [ ./hosts/golem/home ];
        };

        hal = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            nur.modules.nixos.default
            self.nixosModules.common
            self.nixosModules.nixos
            ./hosts/hal/configuration
            { nixpkgs = nixpkgsArgs; }
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        hestia = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            self.nixosModules.common
            self.nixosModules.nixos
            ./hosts/hestia/configuration
            {
              nixpkgs = nixpkgsArgs;
              profiles.home-automation.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        hob = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            nur.modules.nixos.default
            self.nixosModules.common
            self.nixosModules.nixos
            ./hosts/hob/configuration
            {
              nixpkgs = nixpkgsArgs // {
                config.allowUnsupportedSystem = true;
              };
              profiles.home-automation.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        karakuri = self.lib.makeNixosSystem {
          system = "x86_64-linux";
          users = [ "matt" ];
          extraNixosModules = [
            ./hosts/karakuri/configuration
            {
              profiles = {
                desktop.enable = true;
                personal.enable = true;
              };
              home-manager.users.matt.profiles = {
                desktop.enable = true;
                personal.enable = true;
              };
            }
          ];
          extraHmModules = [ ./hosts/karakuri/home ];
        };
      };

      homeConfigurations = {
        matt = nixpkgs.lib.makeOverridable home-manager.lib.homeManagerConfiguration rec {

          pkgs = mkPkgs { system = "x86_64-linux"; };

          extraSpecialArgs = mkExtraSpecialArgs pkgs;

          modules = [
            self.homeManagerModules.common
            self.homeManagerModules.nixos

            stylix.homeManagerModules.stylix
            self.homeManagerModules.stylix

            (
              { config, lib, ... }:
              {
                home = {
                  username = lib.mkDefault "matt";
                  homeDirectory = "/home/${config.home.username}";
                  stateVersion = lib.mkDefault "22.11";
                };
              }
            )
          ];
        };

        "matt@desktop" = self.homeConfigurations.matt.override (old: {
          modules = old.modules ++ [
            {
              profiles = {
                desktop.enable = true;
                personal.enable = true;
              };
            }
          ];
        });

        "matt@macos" = nixpkgs.lib.makeOverridable home-manager.lib.homeManagerConfiguration rec {

          pkgs = mkPkgs { system = "x86_64-darwin"; };

          extraSpecialArgs = mkExtraSpecialArgs pkgs;

          modules = [
            self.homeManagerModules.common

            stylix.homeManagerModules.stylix
            self.homeManagerModules.stylix

            (
              { config, lib, ... }:
              {
                home = {
                  username = lib.mkDefault "matt";
                  homeDirectory = "/Users/${config.home.username}";
                  stateVersion = lib.mkDefault "22.11";
                };

                programs.wezterm.enable = lib.mkForce false; # TODO: broken on x86_64-darwin

                stylix.targets.swaylock.enable = false; # swaylock unsupported on x86_64-darwin; unclear why this is needed
              }
            )
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

    }
    // flake-utils.lib.eachDefaultSystem (system: {

      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          excludes = [ "hardware-configuration\\.nix" ];
          hooks = {
            nixfmt-rfc-style.enable = true;
          };
        };
      };

      devShells.default = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
      };
    });
}
