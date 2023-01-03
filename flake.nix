{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    base16.url = "github:SenchoPens/base16.nix";
    base16.inputs.nixpkgs.follows = "nixpkgs";
    base16-emacs.url = "github:base16-project/base16-emacs";
    base16-emacs.flake = false;
    base16-rofi.url = "github:tinted-theming/base16-rofi";
    base16-rofi.flake = false;
    base16-schemes.url = "github:base16-project/base16-schemes";
    base16-schemes.flake = false;
    base16-tmux.url = "github:tinted-theming/base16-tmux";
    base16-tmux.flake = false;
    copilot-el.url = "github:zerolfx/copilot.el";
    copilot-el.flake = false;
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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
    , unison-nix
    , ...
    } @ inputs:
    let
      inherit (nixpkgs) lib;

      overlays = [
        self.overlays.default
        emacs-overlay.overlay
        nur.overlay
        unison-nix.overlay
      ];

      mkSpecialArgs = nurpkgs: {
        inherit inputs;

        # gross hack to use modules from NUR
        # https://discourse.nixos.org/t/importing-nur-home-manager-modules-in-nix-flakes/16457
        nurNoPkgs = import inputs.nur {
          pkgs = null;
          inherit nurpkgs;
        };
      };
    in
    {
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
                    extraSpecialArgs = mkSpecialArgs
                      (import inputs.nixpkgs { inherit (config.nixpkgs) system; });

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
              { home-manager.users.matt.profiles.personal.enable = true; }
            ];
            extraHmModules = [ ./hosts/golem/home ];
          };

          karakuri = makeNixosSystem {
            system = "x86_64-linux";
            users = [ "matt" ];
            extraNixosModules = [
              ./hosts/karakuri/configuration
              { home-manager.users.matt.profiles.personal.enable = true; }
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
            {
              home.username = "matt";
              home.homeDirectory = "/home/matt";
              home.stateVersion = "22.11";
              profiles.personal.enable = lib.mkDefault true;
            }
          ];
        };

      nixosModules = {
        common = import ./modules/common/nixos;
        nixos = import ./modules/nixos/nixos;
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
            nix-linter.enable = true;
          };
        };
      };

      devShells.default = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
      };
    });
}
