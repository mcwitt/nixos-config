{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  wheel,
}:

buildPythonPackage rec {
  pname = "magicattr";
  version = "0.1.7";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "frmdstryr";
    repo = "magicattr";
    rev = "v${version}";
    hash = "sha256-tS9ZOKS+iSDJOArnf3Fop2nPYwQifDY6PXuqcZ0Cvcs=";
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
