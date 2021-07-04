self: super:
let sources = import ../../nix/sources.nix; in
{
  jupyterWith = import sources.jupyterWith { };
}
