{ pkgs, jupyter, ... }:
let inherit (pkgs) lib stdenv;
in jupyter.kernels.iPythonWith {
  name = "python";
  packages = ps:
    with ps; [
      altair
      beautifulsoup4
      boto
      matplotlib
      numpy
      pandas
      patsy
      pillow
      psycopg2
      pyyaml
      requests
      scikitlearn
      scipy
      seaborn
      spacy
      statsmodels
      typing
    ];
}
