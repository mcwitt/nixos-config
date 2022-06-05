_: super:
{
  iosevka-custom = super.iosevka.override {
    privateBuildPlan = {
      family = "Iosevka Custom";
      spacing = "normal";
      serifs = "sans";
      ligations.enables = [
        "arrow"
        "arrow2"
        "brack-bar"
        "center-ops"
        "eqeq"
        "eqeqeq"
        "exeq"
        "ineq"
        "kern-bars"
        "kern-dotty"
        "logic"
        "plusplus"
        "slasheq"
        "trig"
      ];
    };
    set = "custom";
  };
}
