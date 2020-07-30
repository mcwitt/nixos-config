self: super:
let
  myOverride = {
    packageOverrides = self: super: {

      vulture = super.buildPythonPackage rec {
        pname = "vulture";
        version = "1.6";
        doCheck = false;
        src = super.fetchPypi {
          inherit pname version;
          sha256 = "1sbwbwkpk3s7iwnwsdrvj1ydw9lgbn3xqhji7f8y5y6vvr77i53v";
        };
      };
    };
  };
in {
  # NOTE: Need to add an override for each required python version
  python2 = super.python2.override myOverride;
  python3 = super.python3.override myOverride;
}
