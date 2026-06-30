{
  lib,
  buildNpmPackage,
  fetchurl,
}:

buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.31";

  src = fetchurl {
    url = "https://registry.npmjs.org/pi-acp/-/pi-acp-${version}.tgz";
    hash = "sha256-H+ovaHoIKiNQEZn5OVnpw4oImx9up8whYgIZ4/ovZJE=";
  };

  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-Zoez4AXmLHVQJx3nCTccI+shL+A8OKVOPyKtpX37TE8=";

  dontNpmBuild = true;

  meta = {
    description = "ACP adapter for the pi coding agent";
    homepage = "https://github.com/svkozak/pi-acp";
    license = lib.licenses.mit;
    mainProgram = "pi-acp";
    platforms = lib.platforms.all;
  };
}
