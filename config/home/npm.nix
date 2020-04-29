let
  homeDir = builtins.getEnv "HOME";
  prefix = "${homeDir}/.npm-global";
in {
  home.file.".npmrc".text = ''
    prefix=${prefix}
  '';

  home.sessionVariables = { PATH = "${prefix}/bin:$PATH"; };
}
