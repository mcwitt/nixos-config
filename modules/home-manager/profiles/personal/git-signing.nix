{
  config,
  lib,
  osConfig,
  ...
}:
let
  cfg = config.profiles.personal;

  # Per-host SSH commit-signing keys (public halves; the private keys are
  # generated on each host, passphrase-protected, held by gpg-agent, and never
  # leave it). To enable signing on a new host: ssh-keygen -t ed25519 -f
  # ~/.ssh/git-signing_ed25519, ssh-add it, add the .pub line here, and
  # register it on GitHub as a *signing* key. Hosts not listed simply don't
  # sign. All listed keys are trusted in ~/.ssh/allowed_signers so any host can
  # verify any other's commits.
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
