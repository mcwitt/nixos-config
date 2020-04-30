{ jupyter, ... }:
jupyter.kernels.iPythonWith {
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
      psycopg2
      # pyro-ppl
      pillow
      pytorch
      pyyaml
      requests
      scikitlearn
      scipy
      seaborn
      spacy
      statsmodels
      # tensorflow-bin
      # (tensorflow-probability.override { tensorflow = tensorflow-bin; })
      torchvision
      typing
    ];
}
