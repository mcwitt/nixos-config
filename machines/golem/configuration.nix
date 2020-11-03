{ pkgs, ... }: {
  imports = [ ../../configuration.nix ];

  hardware.printers = rec {
    ensureDefaultPrinter = "Brother_HL-L2340D_series";
    ensurePrinters = [{
      deviceUri =
        "dnssd://Brother%20HL-L2340D%20series._ipp._tcp.local/?uuid=e3248000-80ce-11db-8000-40490f90f0a2";
      model = "drv:///brlaser.drv/brl2340d.ppd";
      name = ensureDefaultPrinter;
      ppdOptions = {
        Duplex = "DuplexNoTumble";
        PageSize = "A4";
      };
    }];
  };

  networking = {
    hostName = "golem";
    interfaces.enp0s31f6.useDHCP = true;
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };

  services.xserver.xrandrHeads = [
    {
      primary = true;
      output = "DP-4";
    }
    {
      monitorConfig = ''
        Option "Rotate" "left"
      '';
      output = "DP-2";
    }
  ];
}
