{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    base16-rofi.url = "github:tinted-theming/base16-rofi";
    base16-rofi.flake = false;
    base16-schemes.url = "github:tinted-theming/base16-schemes";
    base16-schemes.flake = false;
    base16-tmux.url = "github:tinted-theming/base16-tmux";
    base16-tmux.flake = false;
    copilot-el.url = "github:zerolfx/copilot.el";
    copilot-el.flake = false;
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay/be34ec53d305a01049f25b25740aeea6e5fa1005";
    fish-kubectl-completions.url = "github:evanlucas/fish-kubectl-completions";
    fish-kubectl-completions.flake = false;
    "fzf.fish".url = "github:PatrickF1/fzf.fish";
    "fzf.fish".flake = false;
    git-sync-el.url = "github:mcwitt/git-sync.el";
    git-sync-el.flake = false;
    gitignore.url = "github:github/gitignore";
    gitignore.flake = false;
    "gitignore.nix".url = "github:hercules-ci/gitignore.nix";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    stylix.url = "github:danth/stylix/release-22.11";
    unison-nix.url = "github:ceedubs/unison-nix";
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
    , unison-nix
    , ...
    } @ inputs:
    let
      lib = nixpkgs.lib.extend (import ./lib.nix { inherit inputs; });

      overlays = [
        self.overlays.default
        emacs-overlay.overlay
        nur.overlay
        unison-nix.overlay
      ];

      mkSpecialArgs = pkgs: {
        inherit inputs;

        # gross hack to use modules from NUR
        # https://discourse.nixos.org/t/importing-nur-home-manager-modules-in-nix-flakes/16457
        nurNoPkgs = import inputs.nur {
          pkgs = null;
          nurpkgs = pkgs;
        };
      };
    in
    {
      inherit lib;

      overlays.default = import ./overlay.nix { inherit inputs; };

      nixosConfigurations =
        let
          makeNixosSystem = lib.makeOverridable ({ system, users, extraNixosModules, extraHmModules }:
            let
              pkgs = nixpkgs.legacyPackages.${system};
            in
            lib.nixosSystem {
              inherit system;
              specialArgs = { inherit inputs; };
              modules = [
                home-manager.nixosModules.home-manager

                inputs.stylix.nixosModules.stylix
                self.nixosModules.stylix

                self.nixosModules.common
                self.nixosModules.nixos

                ({ config, ... }: {
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
                    extraSpecialArgs = mkSpecialArgs pkgs;

                    users = builtins.listToAttrs (map
                      (user: lib.nameValuePair user {
                        imports = [
                          self.homeManagerModules.common
                          self.homeManagerModules.nixos
                        ] ++ extraHmModules;
                      })
                      users);
                  };
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
              { home-manager.users.matt.profiles = lib.setAll { enable = true; } [ "desktop" "personal" ]; }
            ];
            extraHmModules = [ ./hosts/golem/home ];
          };

          karakuri = makeNixosSystem {
            system = "x86_64-linux";
            users = [ "matt" ];
            extraNixosModules = [
              ./hosts/karakuri/configuration
              { home-manager.users.matt.profiles = lib.setAll { enable = true; } [ "desktop" "personal" ]; }
            ];
            extraHmModules = [ ./hosts/karakuri/home ];
          };
        };

      homeConfigurations."matt@linux" =
        lib.makeOverridable home-manager.lib.homeManagerConfiguration rec {

          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            inherit overlays;
          };

          extraSpecialArgs = mkSpecialArgs pkgs;

          modules = [
            self.homeManagerModules.common
            self.homeManagerModules.nixos

            stylix.homeManagerModules.stylix
            self.nixosModules.stylix

            ({ config, ... }: {
              home = {
                username = lib.mkDefault "matt";
                homeDirectory = "/home/${config.home.username}";
                stateVersion = lib.mkDefault "22.11";
              };
            })
          ];
        };

      homeConfigurations."matt@linux-desktop" = self.homeConfigurations."matt@linux".override (old: {
        modules = old.modules ++ [{ profiles = lib.setAll { enable = lib.mkDefault true; } [ "desktop" "personal" ]; }];
      });

      nixosModules = {
        common = import ./modules/common/nixos;
        nixos = import ./modules/nixos/nixos;
        stylix = import ./modules/stylix;
      };

      homeManagerModules = {
        common = import ./modules/common/home-manager;
        nixos = import ./modules/nixos/home-manager;
        darwin = import ./modules/darwin/home-manager;
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
