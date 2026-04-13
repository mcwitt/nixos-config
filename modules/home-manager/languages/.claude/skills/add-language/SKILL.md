---
name: add-language
description: Add a new language module under modules/home-manager/languages/. Use when the user wants to add support for a new programming language to their home-manager config (editor integration, LSP, formatter, linter, global tooling).
argument-hint: "<language>"
---

# add-language

Add a new home-manager language module under `modules/home-manager/languages/<language>/default.nix` and register it in the sibling `default.nix`.

The module provides editor configuration (primarily Emacs) plus a small set of globally-available tools for ad-hoc work and bootstrapping. **Project-specific tooling is expected to live in a project-local flake**, so keep the global package set minimal — language runtime, package manager, LSP, formatter/linter. Don't pin project-level dependencies here.

## Steps

1. **Research current best practices** (LSP, formatter, linter, package manager, build tool) for the target language as of the current date. The user values *modern* choices — e.g. `ruff` over `black`+`flake8`, `biome` over `eslint`+`prettier`, `vtsls` over `typescript-language-server`. Prefer single-tool solutions when they're mature.

2. **Verify nixpkgs availability and versions** for each tool:
   ```
   nix eval --raw 'nixpkgs#<attr>.version'
   ```
   If a tool is not packaged, fall back to the next-best option rather than adding a custom package.

3. **Read 2–3 existing modules** to match conventions:
   - `python/default.nix` — full-featured (eglot, reformatter, ligature, format-all, vscode, gitignores, options with a `globalPackages` arg)
   - `nix/default.nix` — eglot with custom server-program registration, ts-mode remap, bindLocal
   - `rust/default.nix` / `go/default.nix` — minimal modules
   - `haskell/default.nix` — module with extra options

4. **Write the module** at `modules/home-manager/languages/<language>/default.nix`. Standard skeleton:

   ```nix
   {
     config,
     lib,
     pkgs,
     ...
   }:
   with lib;
   let
     cfg = config.languages.<language>;
   in
   {
     options.languages.<language>.enable = mkEnableOption "<Language> language environment";

     config = mkIf cfg.enable {
       home.packages = with pkgs; [ /* runtime, lsp, formatter, pkg manager */ ];

       programs.emacs.init.usePackage = {
         eglot.hook = [ "(<lang>-ts-mode . eglot-ensure)" ];
         # reformatter for formatter-on-save (see python/default.nix)
         # ligature, subword/superword, format-all disable, mode registration
         <lang>-ts-mode.enable = true;
       };

       programs.git.ignores = pkgs.gitignores "<GitHubGitignoreName>";
     };
   }
   ```

   Things to consider including:
   - **eglot** hook + `eglot-server-programs` entry if the LSP isn't auto-detected
   - **reformatter** definitions for format-on-save and lint-fix-on-save (mirror `python/default.nix`)
   - **format-all** disable hook when reformatter is in use
   - **ligature** config for the mode
   - **subword** or **superword** mode hook
   - **ts-mode registration** via `mode = [ ''("\\.ext\\'" . <lang>-ts-mode)'' ];` if not built-in
   - **major-mode-remap-alist** entry if both a legacy mode and ts-mode exist (see `nix/default.nix`)
   - **`programs.git.ignores = pkgs.gitignores "<Name>"`** using a [github/gitignore](https://github.com/github/gitignore) template name
   - **`programs.vscode.profiles.default.extensions`** wrapped in `mkIf (!pkgs.stdenv.isDarwin)`
   - A **`globalPackages`** option (see `python/default.nix`) only if the language has a meaningful "user packages" concept

5. **Register the module** by adding `./<language>` to the imports list in `modules/home-manager/languages/default.nix` (keep it alphabetically sorted).

6. **Format and verify**:
   ```
   nixfmt modules/home-manager/languages/<language>/default.nix modules/home-manager/languages/default.nix
   git add -N modules/home-manager/languages/<language>/default.nix
   nix eval --no-warn-dirty '.#nixosConfigurations.karakuri.config.home-manager.users.matt.languages.<language>.enable'
   ```
   Then verify the module evaluates with the option enabled:
   ```
   nix eval --no-warn-dirty --impure --expr '
     let
       flake = builtins.getFlake (toString ./.);
       cfg = flake.nixosConfigurations.karakuri.extendModules {
         modules = [{ home-manager.users.matt.languages.<language>.enable = true; }];
       };
     in builtins.length cfg.config.home-manager.users.matt.home.packages
   '
   ```

7. **Do not enable the module by default**. Languages are opt-in per host. Mention to the user where to enable it (host configurations in `flake.nix` or `hosts/<host>/`) but let them make that change.

## Notes

- Treesitter grammars are already available globally via `modules/home-manager/profiles/base/emacs/treesit.nix`, so `<lang>-ts-mode` works without per-language grammar setup.
- The `usePackage` DSL supports: `enable`, `hook`, `config`, `init`, `command`, `mode`, `bindLocal`, `bindKeyMap`. Grep existing modules for examples before guessing.
- Keep global packages minimal. If in doubt, leave it out — the project flake will add it.
- Commit message convention: `languages.<language>: init`.
