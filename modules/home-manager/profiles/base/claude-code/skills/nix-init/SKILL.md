---
name: nix-init
description: Set up a Nix flake project with devShell, package build, and git-hooks. Use when the user wants to create a new Nix project, add a flake to an existing project, or set up a Nix development environment.
argument-hint: "<language> [project-name]"
---

# nix-init

Set up a Nix flake project. The user will specify a language (e.g. `python`, `haskell`) and optionally a project name.

## Overview

Generate a `flake.nix` with:
- A development shell with language tooling and LSP
- A buildable package using the idiomatic Nix builder for the language
- Pre-commit hooks via `cachix/git-hooks.nix` with formatters and static analysis
- A `formatter` output wired to the hooks
- Multi-system support via `nix-systems/default`

Also generate:
- `.envrc` with `use flake` for direnv
- Language project files (using native tooling where possible)

## Flake structure

### Inputs

```nix
inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  systems.url = "github:nix-systems/default";
  git-hooks = {
    url = "github:cachix/git-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};
```

### Multi-system helper

Use `nix-systems/default` with `nixpkgs.lib.genAttrs`. Do NOT use `flake-utils`.

```nix
outputs = { self, nixpkgs, systems, git-hooks, ... }:
let
  forEachSystem = nixpkgs.lib.genAttrs (import systems);
  pkgsFor = system: nixpkgs.legacyPackages.${system};
in
{
  checks = forEachSystem (system: { ... });
  formatter = forEachSystem (system: ...);
  packages = forEachSystem (system: { ... });
  devShells = forEachSystem (system: { ... });
};
```

### Checks (git-hooks)

```nix
checks = forEachSystem (system: {
  pre-commit-check = git-hooks.lib.${system}.run {
    src = ./.;
    hooks = {
      nixfmt.enable = true;
      # ... language-specific hooks
    };
  };
});
```

### Formatter

Wire the formatter to pre-commit so `nix fmt` runs all configured hooks:

```nix
formatter = forEachSystem (system:
  let
    pkgs = pkgsFor system;
    inherit (self.checks.${system}.pre-commit-check.config) package configFile;
  in
  pkgs.writeShellScriptBin "pre-commit-run" ''
    ${pkgs.lib.getExe package} run --all-files --config ${configFile}
  ''
);
```

### DevShell

Inherit `shellHook` and `enabledPackages` from the git-hooks check. Use `inputsFrom` to automatically pull in build dependencies from the package definition — never duplicate dependency lists between the package and shell.

For most languages, use `mkShell` with `inputsFrom`:

```nix
devShells = forEachSystem (system: {
  default =
    let
      pkgs = pkgsFor system;
      inherit (self.checks.${system}.pre-commit-check) shellHook enabledPackages;
    in
    pkgs.mkShell {
      inherit shellHook;
      inputsFrom = [ self.packages.${system}.default ];
      packages = enabledPackages ++ [
        # language server, build tools, etc.
      ];
    };
});
```

For Haskell, use `haskellPackages.shellFor` instead — it properly configures GHC's package database so GHCi and cabal can find library dependencies:

```nix
devShells = forEachSystem (system: {
  default =
    let
      pkgs = pkgsFor system;
      inherit (self.checks.${system}.pre-commit-check) shellHook enabledPackages;
    in
    pkgs.haskellPackages.shellFor {
      packages = _: [ self.packages.${system}.default ];
      withHoogle = true;
      inherit shellHook;
      nativeBuildInputs = enabledPackages ++ [
        pkgs.cabal-install
        pkgs.haskell-language-server
      ];
    };
});
```

## Language-specific patterns

### Python

**Hooks:** `nixfmt`, `ruff` (formatter + linter), `ty` (type checker, custom hook)

**DevShell extras:** `ty` is already in `enabledPackages` from the hook, so it serves double duty as the LSP — no need to add it separately.

**Package:** Use `pkgs.python3Packages.buildPythonApplication` (for executables) or `pkgs.python3Packages.buildPythonPackage` (for libraries). Use modern attribute names: `pyproject = true` (not `format = "pyproject"`), `build-system` (not `nativeBuildInputs` for build backends), `dependencies` (not `propagatedBuildInputs`), and `optional-dependencies` where needed. Read dependencies from `pyproject.toml`.

**Project file:** Generate a `pyproject.toml` with project name, version, and basic structure.

**Custom hooks:** `ty` is not a built-in hook in `git-hooks.nix`, so it must be defined as a custom hook using the module syntax (a function taking `{ config, lib, ... }`). The type checker needs `--python` pointed at a Python interpreter that has the project's dependencies installed, so it can resolve third-party imports. Build this interpreter with `pkgs.python3.withPackages` pulling deps from the package definition.

**Example flake.nix:**

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      systems,
      git-hooks,
      ...
    }:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
      pkgsFor = system: nixpkgs.legacyPackages.${system};
    in
    {
      checks = forEachSystem (system:
        let
          pkgs = pkgsFor system;
        in
        {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt.enable = true;
            ruff.enable = true;
            ty = (
              { config, lib, ... }:
              {
                enable = true;
                package = pkgs.ty;
                entry = "${lib.getExe config.package} check --python ${
                  pkgs.python3.withPackages (
                    _:
                    self.packages.${system}.default.dependencies
                    # ++ self.packages.${system}.default.optional-dependencies.test
                  )
                }/bin/python3";
                types = [ "python" ];
              }
            );
          };
        };
      });

      formatter = forEachSystem (
        system:
        let
          pkgs = pkgsFor system;
          inherit (self.checks.${system}.pre-commit-check.config) package configFile;
        in
        pkgs.writeShellScriptBin "pre-commit-run" ''
          ${pkgs.lib.getExe package} run --all-files --config ${configFile}
        ''
      );

      packages = forEachSystem (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.python3Packages.buildPythonApplication {
            pname = "PROJECT_NAME";
            version = "0.1.0";
            src = ./.;
            pyproject = true;
            build-system = [ pkgs.python3Packages.setuptools ];
            # dependencies = [ ... ];
          };
        }
      );

      devShells = forEachSystem (
        system:
        let
          pkgs = pkgsFor system;
          inherit (self.checks.${system}.pre-commit-check) shellHook enabledPackages;
        in
        {
          default = pkgs.mkShell {
            inherit shellHook;
            inputsFrom = [ self.packages.${system}.default ];
            packages = enabledPackages;
          };
        }
      );
    };
}
```

### Haskell

**Hooks:** `nixfmt`, `hlint`, `ormolu`, `cabal-fmt`

**DevShell extras:** Add `pkgs.haskell-language-server` (not covered by any hook). Add `pkgs.cabal-install` for building.

**Package:** Use `pkgs.haskellPackages.callCabal2nix` to derive the package from the `.cabal` file. Do NOT use stack or raw `mkDerivation`.

**Project file:** Run `cabal init` to generate the `.cabal` file. Use `--non-interactive` with sensible defaults. Specify `--language=GHC2024`.

**Example flake.nix:**

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      systems,
      git-hooks,
      ...
    }:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
      pkgsFor = system: nixpkgs.legacyPackages.${system};
    in
    {
      checks = forEachSystem (system: {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt.enable = true;
            hlint.enable = true;
            ormolu.enable = true;
            cabal-fmt.enable = true;
          };
        };
      });

      formatter = forEachSystem (
        system:
        let
          pkgs = pkgsFor system;
          inherit (self.checks.${system}.pre-commit-check.config) package configFile;
        in
        pkgs.writeShellScriptBin "pre-commit-run" ''
          ${pkgs.lib.getExe package} run --all-files --config ${configFile}
        ''
      );

      packages = forEachSystem (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.haskellPackages.callCabal2nix "PROJECT_NAME" ./. { };
        }
      );

      devShells = forEachSystem (
        system:
        let
          pkgs = pkgsFor system;
          inherit (self.checks.${system}.pre-commit-check) shellHook enabledPackages;
        in
        {
          default = pkgs.haskellPackages.shellFor {
            packages = _: [ self.packages.${system}.default ];
            withHoogle = true;
            inherit shellHook;
            nativeBuildInputs = enabledPackages ++ [
              pkgs.cabal-install
              pkgs.haskell-language-server
            ];
          };
        }
      );
    };
}
```

## Generalizing to other languages

The Python and Haskell examples above are concrete instances of general principles. When setting up any language:

1. **Hooks:** Pick the idiomatic formatter(s) and static analysis tools for the language. Check what's available in [git-hooks.nix](https://github.com/cachix/git-hooks.nix). Always include `nixfmt`.

2. **DevShell:** Use `inputsFrom` to inherit build dependencies from the package definition — never duplicate deps between package and shell. Start with `enabledPackages` from the git-hooks check (this gives you all hook tool binaries for free). Then add:
   - The language server for the language (e.g. `rust-analyzer` for Rust, `gopls` for Go), **unless** it's already provided by one of the enabled hooks (e.g. `ty` for Python doubles as both hook and LSP)
   - Build tools not already included (e.g. `cabal-install` for Haskell, `cargo` for Rust)
   - **Exception:** For Haskell, use `haskellPackages.shellFor` instead of `mkShell` + `inputsFrom`, since it properly sets up GHC's package database.

3. **Package:** Use the idiomatic Nix builder for the language. Use modern attribute names where applicable (e.g. Python: `pyproject = true`, `build-system`, `dependencies` instead of `format = "pyproject"`, `nativeBuildInputs`, `propagatedBuildInputs`).
   - Python: `buildPythonApplication` / `buildPythonPackage`
   - Haskell: `haskellPackages.callCabal2nix`
   - Rust: `rustPlatform.buildRustPackage`
   - Go: `buildGoModule`
   - etc.

4. **Project files:** Generate using native tooling where possible (e.g. `cabal init`, `cargo init`). Fall back to writing the file directly when no tool exists (e.g. `pyproject.toml`).

## .envrc

Always generate a `.envrc` file:

```
use flake
```

## Workflow

1. Determine the language from `$ARGUMENTS` (first argument) and project name (second argument, defaulting to the current directory name).
2. Generate `flake.nix` with the appropriate language configuration. Replace `PROJECT_NAME` with the actual project name.
3. Generate `.envrc`.
4. Generate language project files (using native tools like `cabal init` where available, or writing directly).
5. Initialize a git repo if one doesn't exist (`git init`) — flakes require git.
6. Add generated files to git staging (`git add`) so Nix can see them.
7. Run `nix flake check` to verify the flake evaluates correctly.
8. Keep output minimal — the user is experienced with Nix.
