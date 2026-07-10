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

  config = lib.mkMerge [
    # polybar (X11/xmonad desktop) GPU module. Inert on ewm hosts (polybar off);
    # removed entirely when the desktop profile is dropped.
    (lib.mkIf cfg.enable {
      services.polybar.settings = {
        "module/nvidia-gpu" = {
          type = "custom/script";
          exec = ''
            printf '%2d' "$(${osConfig.hardware.nvidia.package.bin}/bin/nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits)"
          '';
          interval = 1;
          label =
            let
              runTermAppOnClick = bin: label: "%{A1:${lib.getExe pkgs.ghostty} -e ${bin}:}${label}%{A}";
            in
            runTermAppOnClick (lib.getExe pkgs.nvtopPackages.nvidia) "%output%%";

          format-prefix = "GPU ";
        };
      };
    })

    # waybar (ewm/Wayland desktop) GPU indicator. Hosts list "custom/nvidia" in
    # programs.waybarModulesRight to show it.
    (lib.mkIf (cfg.enable && config.profiles.wayland.enable) {
      programs.waybar.settings.mainBar."custom/nvidia" = {
        interval = 2;
        exec = "${osConfig.hardware.nvidia.package.bin}/bin/nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | ${pkgs.gawk}/bin/awk '{printf \"GPU %2d%%\", $1}'";
        on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.nvtopPackages.nvidia}";
      };
    })
  ];
}
