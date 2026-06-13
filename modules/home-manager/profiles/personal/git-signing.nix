{
  config,
  lib,
  osConfig,
  ...
}:
let
  cfg = config.profiles.personal;

  # Per-host commit-signing public keys; the private halves stay on their
  # host (~/.ssh/git-signing_ed25519, loaded with ssh-add). New host: keygen,
  # add the .pub here, register on GitHub as a signing key.
  signingKeys = {
    karakuri = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC7PX69c6rbQTK0oF0dEkaE0KbINOi2i/96WqbkuNN3E matt@karakuri-git-signing";
    satori = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF9fJ7X0eNz6P5hqKq8zTLpaNl4SddsJ9rbEv9iZWFgC matt@satori git signing";
  };
in
{
  config = lib.mkIf cfg.enable {

    programs.git = {
      settings.gpg.ssh.allowedSignersFile = "~/.ssh/allowed_signers";

      signing = lib.mkIf (signingKeys ? ${osConfig.networking.hostName}) {
        format = "ssh";
        key = "key::${signingKeys.${osConfig.networking.hostName}}";
        signByDefault = true;
      };
    };

    home.file.".ssh/allowed_signers" = lib.mkIf (signingKeys != { }) {
      text = lib.concatMapStrings (key: "mcwitt@gmail.com ${key}\n") (lib.attrValues signingKeys);
    };
  };
}
