{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.idris;
in
{
  options.languages.idris.enable = mkEnableOption "Idris language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.idris2 ];

    programs.emacs = {
      overrides = self: super: {
        idris2-mode = super.trivialBuild {
          pname = "idris2-mode";
          src = pkgs.fetchFromGitHub
            {
              owner = "redfish64";
              repo = "idris2-mode";
              rev = "e7cfdfce2c4e6590e697d37e99ba58aae066ef7d";
              sha256 = "03xczm4fzqcg740wj57d1wbqpz5ckqd75k47fhx3ix18cfg4qkp8";
            };
          packageRequires = [ super.prop-menu ];
        };
      };

      init.usePackage.idris2-mode = {
        enable = true;
        mode = [ ''"\\.idr\\'"'' ];
      };
    };
  };
}
