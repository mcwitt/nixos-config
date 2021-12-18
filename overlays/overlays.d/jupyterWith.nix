let sources = import ../../nix/sources.nix; in
_: _: {
  jupyterWith = import sources.jupyterWith { };
}
