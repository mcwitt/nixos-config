{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
    inputs@{ self
    , nixpkgs
    , flake-utils
    , home-manager
    , emacs-overlay
    , nur
    , pre-commit-hooks
    , unison-nix
    , ...
    }: {

      overlay = import ./overlay.nix;

      lib = import ./lib.nix { inherit inputs; inherit (nixpkgs) lib; };

      nixosConfigurations =
        let
          makeNixosSystem = { system, username, extraNixosModules, extraHmModules }:
            nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
              inherit system;
              specialArgs = { inherit inputs; };
              modules = [
                home-manager.nixosModules.home-manager
                self.nixosModules.common
                self.nixosModules.nixos
                ({ config, ... }: {
                  nixpkgs = {
                    config.allowUnfree = true;
                    overlays = [
                      self.overlay
                      emacs-overlay.overlay
                      nur.overlay
                      unison-nix.overlay
                    ];
                  };

                  home-manager = {
                    useGlobalPkgs = true;
                    useUserPackages = true;
                    extraSpecialArgs = {
                      inherit inputs;

                      # gross hack to use modules from NUR
                      # https://discourse.nixos.org/t/importing-nur-home-manager-modules-in-nix-flakes/16457
                      nurNoPkgs = import inputs.nur {
                        pkgs = null;
                        nurpkgs = import inputs.nixpkgs { inherit (config.nixpkgs) system; };
                      };
                    };

                    users.${username} = {
                      imports = [
                        self.homeManagerModules.common
                        self.homeManagerModules.nixos
                      ] ++ extraHmModules;
                      profiles.personal.enable = true;
                    };
                  };
                })
              ] ++ extraNixosModules;
            };
        in
        {
          golem = makeNixosSystem {
            system = "x86_64-linux";
            username = "matt";
            extraNixosModules = [ ./hosts/golem/configuration ];
            extraHmModules = [ ./hosts/golem/home ];
          };

          karakuri = makeNixosSystem {
            system = "x86_64-linux";
            username = "matt";
            extraNixosModules = [ ./hosts/karakuri/configuration ];
            extraHmModules = [ ./hosts/karakuri/home ];
          };
        };

      nixosModules = {
        common = import ./modules/common/nixos;
        nixos = import ./modules/nixos/nixos;
      };

      homeManagerModules = {
        common = import ./modules/common/home-manager;
        nixos = import ./modules/nixos/home-manager;
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

      devShell = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
      };
    });
}
