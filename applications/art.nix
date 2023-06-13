{ config, lib, pkgs, ... }:

let nixGL = import ./nixGL.nix { inherit pkgs config; };
in lib.mkIf config.roles.art {
  home.packages = with pkgs;
    [ ] ++ (lib.lists.optionals config.graphical [ (nixGL krita) ]);
}
