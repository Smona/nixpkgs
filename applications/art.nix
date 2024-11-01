{
  config,
  lib,
  pkgs,
  ...
}:

lib.mkIf config.roles.art {
  home.packages =
    with pkgs;
    [ ] ++ (lib.lists.optionals config.graphical [ (config.lib.nixGL.wrap krita) ]);
}
