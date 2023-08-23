{
  services.home-assistant = {
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = { };
      homeassistant = {
        latitude = 37.8715;
        longitude = -122.2730;
        name = "Home";
      };
    };

    extraComponents = [
      # Components required to complete the onboarding
      "esphome"
      "met"
      "radio_browser"

      "pychromecast"
      "pyflume"
      "zhaquirks"
    ];

    openFirewall = true;
  };
}
