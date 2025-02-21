{ config, lib, ... }:
with lib;
let
  cfg = config.profiles.personal;
in
{
  imports = [ ./email.nix ];

  options.profiles.personal.enable = mkEnableOption "Profile for use on machines I own";

  config = mkIf cfg.enable {
    languages =
      {

        haskell = {
          enable = true;
          hoogle.enable = true;
          globalPackages =
            ps: with ps; [
              aeson
              array
              containers
              lens
              lens-aeson
              linear
              monad-loops
              monoidal-containers
              mtl
              optparse-generic
              parsec
              random-fu
              raw-strings-qq
              rvar
              safe
              split
              streaming
              tasty
              tasty-hunit
              text
              turtle
              vector
            ];
        };

        python = {
          enable = true;
          globalPackages =
            ps: with ps; [
              httpx
              hypothesis
              matplotlib
              notebook
              pandas
              scipy
              seaborn
              sympy
              toolz
            ];
        };
      }
      // lib.genAttrs
        [
          "R"
          "agda"
          # "coq"
          "cpp"
          "cuda"
          "dhall"
          "elm"
          "go"
          "graphviz"
          # "idris"
          "js"
          "markdown"
          "plantuml"
          "rust"
          "scala"
          "shell"
          "sql"
          "terraform"
          "tex"
        ]
        (_: {
          enable = true;
        });

    programs.git = {
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      signing = {
        key = "981FF1A67C955E2E";
        signByDefault = true;
      };
    };

    programs.texlive = {
      enable = true;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          scheme-basic

          # packages required for Org Mode latex export (see org-latex-default-packages-alist documentation)
          dvipng
          dvisvgm
          wrapfig
          ulem
          amsmath
          capt-of
          hyperref

          # not mentioned but required
          minted
          newfloat

          # not packaged nixpkgs.texlive?
          # inputenc fontenc graphicx longtable rotating amssymb
          ;
      };
    };

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
