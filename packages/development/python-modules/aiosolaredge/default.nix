{ lib
, buildPythonPackage
, fetchFromGitHub
, poetry-core
, aiohttp
, yarl
}:

buildPythonPackage rec {
  pname = "aiosolaredge";
  version = "0.2.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "bdraco";
    repo = "aiosolaredge";
    rev = "v${version}";
    hash = "sha256-1C74U5HWDTJum1XES21t1uIJwm0YW3l041mwvqY/dA4=";
  };

  nativeBuildInputs = [
    poetry-core
  ];

  propagatedBuildInputs = [
    aiohttp
    yarl
  ];

  pythonImportsCheck = [ "aiosolaredge" ];

  meta = with lib; {
    description = "Asyncio SolarEdge";
    homepage = "https://github.com/bdraco/aiosolaredge";
    changelog = "https://github.com/bdraco/aiosolaredge/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
