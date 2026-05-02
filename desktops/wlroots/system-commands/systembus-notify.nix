{ pkgs, ... }:

# TODO: see how hard it would be to package this as a noctalia plugin
let
  pythonWithDbus = pkgs.python3.withPackages (ps: [
    ps.dbus-python
    ps.pygobject3
  ]);
  noctalia-systembus-notify = pkgs.writeShellApplication {
    name = "noctalia-systembus-notify";
    runtimeInputs = [ pythonWithDbus ];
    text = ''
      exec ${pythonWithDbus}/bin/python3 \
        ${./systembus-notify.py}
    '';
  };
in
{
  packages = {
    inherit noctalia-systembus-notify;
  };
}
