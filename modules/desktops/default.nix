# Desktop environment / compositor selection.

{ den, lib, ... }:
{
  den.aspects.desktop = { host, ... }: {
    includes =
      with den.aspects;
      (
        [
          wlroots
        ]
        ++ (lib.lists.optional (host.desktop.shell == "noctalia") noctalia)
        ++ (lib.lists.optional (host.desktop.shell == "custom") custom-shell)
        ++ (lib.lists.optional (host.desktop.shell == "gnome") gnome)
      );

    nixos =
      {
        host,
        config,
        lib,
        ...
      }:
      {
        config = {
          programs.hyprland.enable = host.desktop.compositor == "hyprland";
          programs.niri.enable = host.desktop.compositor == "niri";
          programs.sway.enable = host.desktop.compositor == "sway";
        };
      };
  };
}
