{
  lib,
  buildNpmPackage,
  fetchurl,
}:

buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.33";

  src = fetchurl {
    url = "https://registry.npmjs.org/pi-acp/-/pi-acp-${version}.tgz";
    hash = "sha256-n964pngMBWsywHJC81kIRHIAcwjhq1d1fzM53ZYw3ks=";
  };

  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-A7Ap4pgxUaVTQuKio6NR65tGJNBH1ZOvO0ZGt+Q2bcY=";

  dontNpmBuild = true;

  meta = {
    description = "ACP adapter for the pi coding agent";
    homepage = "https://github.com/svkozak/pi-acp";
    license = lib.licenses.mit;
    mainProgram = "pi-acp";
    platforms = lib.platforms.all;
  };
}
