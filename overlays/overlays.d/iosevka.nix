_: super:
{
  iosevka-custom = super.iosevka.override {
    privateBuildPlan = {
      family = "Iosevka Custom";
      spacing = "normal";
      serifs = "sans";
      ligations.inherits = "haskell";
    };
    set = "custom";
  };
}
