# Desktop environment / compositor selection.
{ ... }:
{
  flake.nixosModules.desktop =
    { config, lib, ... }:
    {
      imports = [
        ../nixos/wlroots.nix
      ];

      # This uses an enum since having multiple enabled at once can cause
      # issues (e.g. running multiple xdg-desktop portals or PAM agents).
      options.smona.desktop.compositor = lib.mkOption {
        description = "Which desktop environment / compositor to enable for this host.";
        type = lib.types.enum [
          "none"
          "gnome"
          "hyprland"
          "niri"
          "sway"
        ];
        default = "none";
      };

      config = {
        programs.hyprland.enable = config.smona.desktop.compositor == "hyprland";
        programs.niri.enable = config.smona.desktop.compositor == "niri";
        programs.sway.enable = config.smona.desktop.compositor == "sway";

        # Propagate the choice into every home-manager user on this host so
        # home-manager modules can hook off the same option.
        home-manager.sharedModules = [
          { smona.desktop.compositor = config.smona.desktop.compositor; }
        ];
      };
    };

  flake.homeModules.desktop =
    { lib, ... }:
    {
      imports = [
        ../desktops/gnome
        ../desktops/wlroots
      ];

      options.smona.desktop.compositor = lib.mkOption {
        description = "Which desktop environment / compositor this user is running.";
        type = lib.types.enum [
          "none"
          "gnome"
          "hyprland"
          "niri"
          "sway"
        ];
        default = "none";
      };
    };
}
