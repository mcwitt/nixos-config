self: super: {
  ghcide = import (super.fetchFromGitHub {
    owner = "cachix";
    repo = "ghcide-nix";
    rev = "f940ec611cc6914693874ee5e024eba921cab19e";
    sha256 = "0vri0rivdzjvxrh6lzlwwkh8kzxsn82jp1c2w5rqzhp87y6g2k8z";
  }) { };
}
