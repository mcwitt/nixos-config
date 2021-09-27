self: super:
let nixpkgs = import
  (super.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "f48b16f4522801f04d2e97bb5fd20ac2ee87d5da";
    sha256 = "1qldbq4fc6mw5xmd1jzlf9ba74jzpkqkdibv0pmayqqlpqiscivq";
  })
  { config.allowUnfree = true; }; in
{
  inherit (nixpkgs) zoom-us;
}
