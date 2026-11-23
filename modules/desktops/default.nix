# Desktop environment / compositor selection.
#
# TODO: refactor to use den host schema + conditional imports.
{ den, ... }:
{
  den.aspects.desktop.includes = with den.aspects; [
    noctalia
    gnome
    custom-shell
    wlroots
  ];

  den.aspects.desktop.nixos =
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
}
