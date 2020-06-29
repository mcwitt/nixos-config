self: super:
let inherit (self.mypkgs) sources;
in { nur = import sources.nur { pkgs = super; }; }
