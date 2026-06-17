{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    autoresearch = {
      url = "github:uditgoenka/autoresearch";
      flake = false;
    };
    claude-code.url = "github:sadjow/claude-code-nix";
    codex-cli.url = "github:sadjow/codex-cli-nix";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    llm-agents.url = "github:numtide/llm-agents.nix";
    llm-agents.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/main";
    nur.url = "github:nix-community/NUR";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    stylix.url = "github:danth/stylix/release-26.05";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    superpowers = {
      url = "github:obra/superpowers";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      disko,
      emacs-overlay,
      flake-utils,
      home-manager,
      nix-darwin,
      nixos-raspberrypi,
      nur,
      pre-commit-hooks,
      stylix,
      ...
    }@inputs:
    {
      overlays.default = import ./overlay { inherit inputs; };

      lib.makeNixosSystem = nixpkgs.lib.makeOverridable (
        {
          system,
          users,
          extraNixosModules ? [ ],
          extraHmModules ? [ ],
          extraSpecialArgs ? { },
          extraExtraSpecialArgs ? { },
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;

          specialArgs = {
            inherit inputs;
          }
          // extraSpecialArgs;

          modules = [
            home-manager.nixosModules.home-manager
            nur.modules.nixos.default

            stylix.nixosModules.stylix
            self.nixosModules.stylix
            ./modules/stylix/linux.nix
            ./modules/stylix/dark.nix

            self.nixosModules.default

            (
              {
                config,
                lib,
                pkgs,
                ...
              }:
              {
                nixpkgs = {
                  overlays = [
                    self.overlays.default
                    emacs-overlay.overlay
                    nur.overlays.default
                    inputs.claude-code.overlays.default
                    inputs.codex-cli.overlays.default
                    inputs.llm-agents.overlays.default
                  ];
                  config.allowUnfree = true;
                };

                users.users = builtins.listToAttrs (
                  map (
                    user:
                    lib.nameValuePair user {
                      isNormalUser = true;
                      extraGroups = [
                        "wheel"
                        "podman"
                        "video"
                      ];
                      shell = pkgs.fish;
                    }
                  ) users
                );

                home-manager = {
                  backupFileExtension = "backup-before-home-manager";

                  extraSpecialArgs = {
                    inherit inputs;

                    # gross hack to use modules from NUR
                    # https://discourse.nixos.org/t/importing-nur-home-manager-modules-in-nix-flakes/16457
                    nurNoPkgs = import nur {
                      pkgs = null;
                      nurpkgs = pkgs;
                    };
                  }
                  // extraExtraSpecialArgs;

                  useGlobalPkgs = true;

                  users = builtins.listToAttrs (
                    map (
                      user:
                      lib.nameValuePair user {
                        imports = [
                          self.homeModules.default
                        ]
                        ++ extraHmModules;

                        profiles.base.enable = true;
                      }
                    ) users
                  );

                  useUserPackages = true;
                };

                profiles.base.enable = true;

                stylix.enable = true;
              }
            )
          ]
          ++ extraNixosModules;
        }
      );

      lib.makeDarwinSystem = nixpkgs.lib.makeOverridable (
        {
          system,
          users,
          extraDarwinModules ? [ ],
          extraHmModules ? [ ],
          extraSpecialArgs ? { },
          extraExtraSpecialArgs ? { },
        }:
        nix-darwin.lib.darwinSystem {
          inherit system;

          specialArgs = {
            inherit inputs;
          }
          // extraSpecialArgs;

          modules = [
            home-manager.darwinModules.home-manager
            ./modules/nixos/nix-community-cache.nix

            stylix.darwinModules.stylix
            self.nixosModules.stylix

            (
              {
                config,
                lib,
                pkgs,
                ...
              }:
              {
                nixpkgs = {
                  overlays = [
                    self.overlays.default
                    emacs-overlay.overlay
                    nur.overlays.default
                    inputs.claude-code.overlays.default
                    inputs.codex-cli.overlays.default
                    inputs.llm-agents.overlays.default
                  ];
                  config.allowUnfree = true;
                };

                users.users = builtins.listToAttrs (
                  map (user: lib.nameValuePair user { home = "/Users/${user}"; }) users
                );

                system.primaryUser = builtins.head users;

                programs.fish.enable = true;

                home-manager = {
                  backupFileExtension = "backup-before-home-manager";

                  extraSpecialArgs = {
                    inherit inputs;

                    # gross hack to use modules from NUR
                    # https://discourse.nixos.org/t/importing-nur-home-manager-modules-in-nix-flakes/16457
                    nurNoPkgs = import nur {
                      pkgs = null;
                      nurpkgs = pkgs;
                    };
                  }
                  // extraExtraSpecialArgs;

                  useGlobalPkgs = true;

                  users = builtins.listToAttrs (
                    map (
                      user:
                      lib.nameValuePair user {
                        imports = [
                          self.homeModules.default
                        ]
                        ++ extraHmModules;

                        profiles.base.enable = true;

                        targets.darwin.copyApps.enable = true;
                        targets.darwin.linkApps.enable = false;
                      }
                    ) users
                  );

                  useUserPackages = true;
                };

                stylix.enable = true;
              }
            )
          ]
          ++ extraDarwinModules;
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
                cuda.enable = true;
                desktop.enable = true;
                personal.enable = true;
                nvidia.enable = true;
              };

              home-manager.users.matt.profiles = {
                desktop.enable = true;
                nvidia.enable = true;
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
            self.nixosModules.default
            ./hosts/hal/configuration
            {
              nixpkgs = {
                overlays = [ self.overlays.default ];
                config.allowUnfree = true;
              };
            }
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        hestia = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            self.nixosModules.default
            ./hosts/hestia/configuration
            {
              nixpkgs = {
                overlays = [ self.overlays.default ];
                config.allowUnfree = true;
              };
            }
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        hob = nixpkgs.lib.makeOverridable nixos-raspberrypi.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            disko.nixosModules.disko
            nur.modules.nixos.default
            self.nixosModules.default
            ./hosts/hob/configuration
            {
              nixpkgs = {
                overlays = [ self.overlays.default ];
                config = {
                  allowUnfree = true;
                  allowUnsupportedSystem = true;
                };
              };

              profiles.home-automation.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs;
            inherit (inputs) nixos-raspberrypi;
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

        satori = self.lib.makeNixosSystem {
          system = "x86_64-linux";
          users = [ "matt" ];
          extraNixosModules = [
            ./hosts/satori/configuration
            {
              nix.settings = {
                substituters = [ "https://nixos-raspberrypi.cachix.org" ];
                trusted-public-keys = [
                  "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
                ];
              };

              profiles = {
                cuda.enable = true;
                desktop.enable = true;
                personal.enable = true;
                nvidia.enable = true;
              };

              home-manager.users.matt.profiles = {
                desktop.enable = true;
                distrobox.enable = true;
                nvidia.enable = true;
                personal.enable = true;
              };
            }
          ];
          extraHmModules = [ ./hosts/satori/home ];
        };
      };

      nixosModules = {
        default = import ./modules/nixos;
        stylix = import ./modules/stylix;
      };

      homeModules = {
        default = import ./modules/home-manager;
        stylix = import ./modules/stylix;
      };

    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            excludes = [ "hardware-configuration\\.nix" ];
            hooks = {
              nixfmt.enable = true;
              hlint.enable = true;
              ormolu.enable = true;
            };
          };
        };

        formatter =
          let
            inherit (self.checks.${system}.pre-commit-check.config) package configFile;
            script = ''
              ${pkgs.lib.getExe package} run --all-files --config ${configFile}
            '';
          in
          pkgs.writeShellScriptBin "pre-commit-run" script;

        devShells.default = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
        };
      }
    );
}
