# Desktop compositor selection.

{ den, lib, ... }:
{
  den.aspects.desktop = { host, ... }: {
    includes =
      with den.aspects;
      (lib.lists.optional (host.desktop.compositor == "gnome") gnome)
      ++ (lib.lists.optional (host.desktop.compositor == "hyprland") hyprland)
      ++ (lib.lists.optional (host.desktop.compositor == "niri") niri)
      ++ (lib.lists.optional (host.desktop.compositor == "sway") sway);
  };
}
