{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  claude-code,
}:

buildNpmPackage (finalAttrs: {
  pname = "claude-agent-acp";
  version = "0.61.0";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-pzvq4hz+rh2tIHRThayqKSDh7NWaE/3kJnaSgpA0zi4=";
  };

  npmDepsHash = "sha256-fX1czUtSB7xHFnMBprWzU+v9zQiSwXfY621f1E0HSR4=";

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/claude-agent-acp \
      --set-default CLAUDE_CODE_EXECUTABLE ${lib.getExe claude-code}
  '';

  meta = {
    description = "ACP-compatible coding agent powered by the Claude Agent SDK";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [
      amadejkastelic
      storopoli
    ];
    mainProgram = "claude-agent-acp";
  };
})
