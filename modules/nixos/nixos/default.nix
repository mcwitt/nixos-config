{
  imports = [ ./cuda.nix ./profiles ];

  # protect nix-shell environments from garbage collection
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '';
}
