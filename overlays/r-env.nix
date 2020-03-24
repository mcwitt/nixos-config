self: super: {
  rEnv = super.rWrapper.override {
    packages = with super.rPackages; [ ggplot2 dplyr xts ];
  };
}
