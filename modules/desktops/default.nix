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
    { config, lib, ... }:
    {
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
      options.smona.desktop.shell = lib.mkOption {
        description = "Which UI shell / panel suite to run on top of the compositor.";
        type = lib.types.enum [
          "none"
          "custom"
          "noctalia"
          "gnome"
        ];
        default = "none";
      };

      config = {
        programs.hyprland.enable = config.smona.desktop.compositor == "hyprland";
        programs.niri.enable = config.smona.desktop.compositor == "niri";
        programs.sway.enable = config.smona.desktop.compositor == "sway";

        # Propagate the choices into every home-manager user on this host so
        # home-manager modules can hook off the same options.
        home-manager.sharedModules = [
          {
            smona.desktop.compositor = config.smona.desktop.compositor;
            smona.desktop.shell = config.smona.desktop.shell;
          }
        ];
      };
    };

  den.aspects.desktop.hmLinux =
    { lib, ... }:
    {
      imports = [
        ../../desktops/wlroots
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
      options.smona.desktop.shell = lib.mkOption {
        description = "Which UI shell / panel suite this user is running.";
        type = lib.types.enum [
          "none"
          "custom"
          "noctalia"
          "gnome"
        ];
        default = "none";
      };
    };
}
