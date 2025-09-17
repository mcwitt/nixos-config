{
  imports = [ ./profiles ];

  # Add MemTest86+ to the systemd-boot menu
  boot.loader.systemd-boot.memtest86.enable = true;
}
