{
  lib,
  buildNpmPackage,
  fetchurl,
}:

buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.34";

  src = fetchurl {
    url = "https://registry.npmjs.org/pi-acp/-/pi-acp-${version}.tgz";
    hash = "sha256-Szv2vHG+0rf0Pdf8+hkG5GSCjkcXQNpZMQfk8WCPJlA=";
  };

  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-KAkd2mf735KeETTmtb8PC/ogmGZ8V/b/kT8eYm/HQ+8=";

  dontNpmBuild = true;

  meta = {
    description = "ACP adapter for the pi coding agent";
    homepage = "https://github.com/svkozak/pi-acp";
    license = lib.licenses.mit;
    mainProgram = "pi-acp";
    platforms = lib.platforms.all;
  };
}
