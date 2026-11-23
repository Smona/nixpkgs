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
        programs.hyprland.enable = host.desktop.compositor == "hyprland";
        programs.niri.enable = host.desktop.compositor == "niri";
        programs.sway.enable = host.desktop.compositor == "sway";

        # Propagate the choices into every home-manager user on this host so
        # home-manager modules can hook off the same options.
        home-manager.sharedModules = [
          {
            smona.desktop.shell = config.smona.desktop.shell;
          }
        ];
      };
    };

  den.aspects.desktop.hmLinux =
    { lib, ... }:
    {
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
