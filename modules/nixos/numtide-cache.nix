{
  # Binary cache for the numtide/llm-agents.nix overlay (e.g. pkgs.llm-agents.pi),
  # whose daily-built packages are not in cache.nixos.org.
  nix.settings = {
    substituters = [
      "https://cache.numtide.com"
    ];
    trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };
}
