{
  imports = [ ./cuda.nix ];

  # protect nix-shell environments from garbage collection
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';
}
