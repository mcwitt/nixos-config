{
  lib,
  stdenvNoCC,
  fetchurl,
  makeWrapper,
  unzip,
}:

let
  # Archive names follow the ACP registry entry:
  # https://github.com/agentclientprotocol/registry/blob/main/antigravity-acp/agent.json
  sources = {
    x86_64-linux = {
      platform = "linux/agy-acp-server-${version}-linux-x86_64";
      hash = "sha256-+7LVTdlPuJeEUwjTtrRhh8Yn8cYAeN7YGt7q+LkMXXo=";
    };
    aarch64-linux = {
      platform = "linux/agy-acp-server-${version}-linux-arm64";
      hash = "sha256-PcQ6IfVAKyOk26ezIKZ0Xp2/S26tkSzp/HYBHqOgin4=";
    };
    x86_64-darwin = {
      platform = "macos/agy-acp-server-${version}-darwin-x86_64";
      hash = "sha256-QHtrjvFnuyB9Hv/b1Ii6z2O6fasrR9SkfY6jcQqMD1U=";
    };
    aarch64-darwin = {
      platform = "macos/agy-acp-server-${version}-darwin-arm64";
      hash = "sha256-IaMu6rhbOUpRVRDIRTm9pLJQS/KRfgogH1xBMLE1gaU=";
    };
  };

  version = "1.4.0";

  source =
    sources.${stdenvNoCC.hostPlatform.system}
      or (throw "agy-acp-server: unsupported system ${stdenvNoCC.hostPlatform.system}");
in
stdenvNoCC.mkDerivation {
  pname = "agy-acp-server";
  inherit version;

  src = fetchurl {
    url = "https://dl.google.com/agy-extensions/releases/${source.platform}.zip";
    inherit (source) hash;
  };

  sourceRoot = ".";

  nativeBuildInputs = [
    makeWrapper
    unzip
  ];

  # agy_acp_server.par is a launcher with its Python runtime appended as a zip
  # archive. Stripping or patchelf would rewrite the executable and orphan the
  # archive, so install it untouched; on NixOS it runs through nix-ld.
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 -t $out/libexec/agy-acp-server agy_acp_server.par localharness_external
    makeWrapper $out/libexec/agy-acp-server/agy_acp_server.par $out/bin/agy_acp_server.par \
      --set-default ANTIGRAVITY_HARNESS_PATH $out/libexec/agy-acp-server/localharness_external

    runHook postInstall
  '';

  meta = {
    description = "Agent Client Protocol server for Google Antigravity";
    homepage = "https://antigravity.google/docs/ide/extensions";
    license = lib.licenses.unfree;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    mainProgram = "agy_acp_server.par";
    platforms = builtins.attrNames sources;
  };
}
