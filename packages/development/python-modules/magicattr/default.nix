{ lib
, buildPythonPackage
, fetchFromGitHub
, setuptools
, wheel
}:

buildPythonPackage rec {
  pname = "magicattr";
  version = "0.1.6";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "frmdstryr";
    repo = "magicattr";
    rev = "v${version}";
    hash = "sha256-hV425AnXoYL3oSYMhbXaF8VRe/B1s5f5noAZYz4MMwc=";
  };

  nativeBuildInputs = [
    setuptools
    wheel
  ];

  pythonImportsCheck = [ "magicattr" ];

  meta = with lib; {
    description = "A getattr and setattr that works on nested objects, lists, dicts, and any combination thereof without resorting to eval";
    homepage = "https://github.com/frmdstryr/magicattr";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
