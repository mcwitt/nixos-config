---
name: nixify
description: Set up a Nix flake project with devShell, package build, and git-hooks. Use when the user wants to create a new Nix project from scratch, add a flake.nix to an existing source repo that does not use Nix, or otherwise set up a Nix development environment.
argument-hint: "<language> [project-name]"
---

# nixify

Set up a Nix flake project. Handles two scenarios:

1. **New project from scratch** — no source code yet. Generate the flake plus language project files.
2. **Existing non-Nix repo** — source code already exists (e.g. `pyproject.toml`, `*.cabal`, `Cargo.toml`, `go.mod`). Generate only the flake and `.envrc`, reading project metadata from the existing files.

The user will specify a language (e.g. `python`, `haskell`) and optionally a project name. If arguments are omitted and the current directory is an existing repo, infer the language from project files and the name from the existing manifest.

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

First, decide which scenario applies by checking the current directory:

- **New project** if the directory is empty or contains only scaffolding (e.g. `README.md`, `LICENSE`, `.git/`).
- **Existing repo** if there are language project files (`pyproject.toml`, `setup.py`, `*.cabal`, `stack.yaml`, `Cargo.toml`, `go.mod`, `package.json`, etc.) or source files in the language.

If `$ARGUMENTS` supplies the language, use that; otherwise infer from project files. If the project name is not supplied, take it from the existing manifest (`pyproject.toml` `[project].name`, `*.cabal` `name:`, `Cargo.toml` `[package].name`, module path in `go.mod`, etc.) or fall back to the current directory name.

Before generating files, confirm you won't overwrite existing ones: if `flake.nix` or `.envrc` already exist, stop and ask the user — do not clobber. Refuse to proceed on a repo that already has a flake.

### New project from scratch

1. Generate `flake.nix` with the appropriate language configuration. Replace `PROJECT_NAME` with the actual project name.
2. Generate `.envrc` with `use flake`.
3. Generate language project files (using native tools like `cabal init` where available, or writing directly).
4. Initialize a git repo if one doesn't exist (`git init`) — flakes require git.
5. Add generated files to git staging (`git add`) so Nix can see them.
6. Run `nix flake check` to verify the flake evaluates correctly.
7. Keep output minimal — the user is experienced with Nix.

### Existing non-Nix repo

1. Read the existing project manifest to determine:
   - Project name and version (copy into `flake.nix`; don't hardcode `0.1.0` if a real version exists).
   - Build system / backend (e.g. `setuptools` vs `hatchling` vs `poetry-core` for Python) — feed this into the package's `build-system` / equivalent.
   - Dependencies — prefer deriving them from the manifest at build time where the builder supports it (e.g. `haskellPackages.callCabal2nix` reads `.cabal` automatically; `buildPythonApplication` with `pyproject = true` reads `pyproject.toml` but you still list runtime `dependencies` explicitly from `[project].dependencies`). Don't duplicate dependency lists beyond what the builder requires.
2. Generate `flake.nix` using the same structure and conventions as for new projects (multi-system via `nix-systems/default`, git-hooks checks, formatter wired to hooks, devShell via `inputsFrom` or `shellFor`). The conventions above apply identically — do not branch on "existing vs new" inside the flake.
3. Generate `.envrc` with `use flake` (only if one doesn't already exist; if direnv is already configured differently, ask the user).
4. Do NOT generate language project files — they already exist.
5. Do NOT run `git init` — assume an existing repo. If by chance there is none, ask the user before initializing.
6. Stage the generated files with `git add flake.nix .envrc` so Nix can see them.
7. Run `nix flake check` to verify the flake evaluates and that pre-commit hooks are happy with the existing source. If hooks fail on pre-existing code (e.g. `ruff`, `ormolu`), report this to the user rather than silently reformatting their codebase — offer to run the formatter as a separate, reviewable step.
8. Keep output minimal — the user is experienced with Nix.
