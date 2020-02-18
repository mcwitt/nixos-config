self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "54f9528d2ae88ba8b3027d391fe8701041201506";
    sha256 = "0zdh4h776m47pq5krlslbnypcx55i5y7wkn47bdw3rl02c5pmbyv";
  });
}
