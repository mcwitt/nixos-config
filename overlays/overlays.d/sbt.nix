self: super: {
  sbt = super.sbt.overrideAttrs (_: { doInstallCheck = !super.stdenv.isDarwin; });
}
