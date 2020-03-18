self: super: {
  all-hies = import (super.fetchFromGitHub {
    owner = "infinisil";
    repo = "all-hies";
    rev = "4b6aab017cdf96a90641dc287437685675d598da";
    sha256 = "0ap12mbzk97zmxk42fk8vqacyvpxk29r2wrnjqpx4m2w9g7gfdya";
  }) { };
}
