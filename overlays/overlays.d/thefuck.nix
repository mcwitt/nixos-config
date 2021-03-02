self: super: {
  thefuck = super.thefuck.overrideAttrs (_: { doInstallCheck = !super.stdenv.isDarwin; });
}
