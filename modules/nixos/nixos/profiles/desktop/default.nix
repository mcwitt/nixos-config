{
  imports = [ ./pipewire.nix ];

  # required for `gtk.enable = true` in home-manager
  programs.dconf.enable = true;
}
