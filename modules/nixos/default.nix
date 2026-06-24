{
  imports = [
    ./codex-cli-cache.nix
    ./nix-community-cache.nix
    ./numtide-cache.nix
    ./tailscale-shutdown.nix
    ./profiles
  ];

  # Add MemTest86+ to the systemd-boot menu
  boot.loader.systemd-boot.memtest86.enable = true;
}
