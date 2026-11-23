{
  inputs,
  den,
  lib,
  ...
}:

let
  systemOptions = {
    # This uses an enum since having multiple enabled at once can cause
    # issues (e.g. running multiple xdg-desktop portals or PAM agents).
    options.desktop.compositor = lib.mkOption {
      description = "Which desktop environment / compositor to enable for this host.";
      type = lib.types.enum [
        "none"
        "gnome"
        "hyprland"
        "niri"
        "sway"
      ];
    };

    options.isGraphical = lib.mkOption {
      description = "Whether this system supports GUI functionality";
      type = lib.types.bool;
    };
  };
in
{
  imports = [ inputs.den.flakeModule ];

  den.schema = {
    user = {
      classes = lib.mkDefault [ "homeManager" ];
    };

    host =
      { host, lib, ... }:
      (
        {
          config = {
            desktop.compositor = lib.mkDefault "none";
            # Linux needs a compositor, darwin is always graphical.
            isGraphical = lib.mkDefault (
              if (lib.systems.elaborate host.system).isLinux then host.desktop.compositor != "none" else true
            );

            includes = [
              # Allow my own user to configure nixos options across all hosts.
              (den.batteries.user-aspects {
                users = [
                  "mel"
                  "smona"
                ];
              })
            ];
          };

        }
        // systemOptions
      );

    home =
      { host, ... }:
      (
        {
          config = {
            desktop.compositor = lib.mkDefault "none";
            # Linux needs a compositor, darwin is always graphical.
            config.isGraphical = lib.mkDefault (
              if (lib.systems.elaborate host.system).isLinux then host.desktop.compositor != "none" else true
            );
          };
        }
        // systemOptions
      );
  };

  den.default = {
    includes = [
      den.batteries.define-user
      den.batteries.hostname
    ];

    # Setting consistent global stateVersion defaults, using the oldest
    # stateVersion in the repo.
    #
    # Before changing these values read their documentation
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html),
    # and perform any necessary migrations on all hosts.
    nixos.system.stateVersion = lib.mkDefault "22.05";
    homeManager.home.stateVersion = lib.mkDefault "22.05";
    darwin.system.stateVersion = lib.mkDefault 4;
  };
}
