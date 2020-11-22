self: super:
let nixpkgs = super.fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "0a626cd0c08b0b23262c5ee0ca34ddb7eca75d87";
  sha256 = "1l51fr1jygb6ds01p621nidgv2cfixkxv284bp76aivdmh6j0zp8";
};
in
{
  awscli2 = super.callPackage "${nixpkgs}/pkgs/tools/admin/awscli" { };
}
