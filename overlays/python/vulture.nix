{ buildPythonPackage, fetchPypi }:
buildPythonPackage rec {
  pname = "vulture";
  version = "1.6";
  doCheck = false;
  src = fetchPypi {
    inherit pname version;
    sha256 = "1sbwbwkpk3s7iwnwsdrvj1ydw9lgbn3xqhji7f8y5y6vvr77i53v";
  };
}
