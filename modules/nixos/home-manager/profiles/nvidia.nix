{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:
let
  cfg = config.profiles.nvidia;
in
{
  options.profiles.nvidia.enable = lib.mkEnableOption "Enable nvidia tools";

  config = lib.mkIf cfg.enable {

    services.polybar.settings = {
      "module/nvidia-gpu" = {
        type = "custom/script";
        exec = ''
          ${osConfig.hardware.nvidia.package.bin}/bin/nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits
        '';
        interval = 1;
        label =
          let
            runTermAppOnClick = bin: label: "%{A1:${lib.getExe pkgs.wezterm} start ${bin}:}${label}%{A}";
          in
          runTermAppOnClick (lib.getExe pkgs.nvtopPackages.nvidia) "GPU %output%%";
      };
    };
  };
}
