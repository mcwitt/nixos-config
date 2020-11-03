{ config, ... }:
let prefix = "${config.home.homeDirectory}/.npm-global";
in {
  home.file.".npmrc".text = ''
    prefix=${prefix}
  '';

  home.sessionVariables = { PATH = "${prefix}/bin:$PATH"; };
}
