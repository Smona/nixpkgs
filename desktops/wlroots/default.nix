{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.smona.wlroots;
  cmd = import ./system-commands { inherit pkgs inputs; };
  my_rofi = pkgs.rofi-wayland.override { plugins = with pkgs; [ rofi-calc ]; };
in
{
  imports = [
    ../../applications/gui.nix
    ./waybar
    ./eww.nix
    ./sway.nix
    ./hyprland.nix
  ];

  options.smona.wlroots = {
    enable = lib.mkEnableOption "wlroots window managers";
    builtInDisplay = lib.mkOption {
      description = "Which monitor ID represents the builtin screen. Get the ID via `swaymsg -t get_outputs`";
      type = lib.types.str;
      default = "";
    };
    primaryMonitor = lib.mkOption {
      description = "Which monitor ID represents the 'primary' monitor.";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    graphical = true;

    smona.waybar.enable = true;
    smona.eww.enable = true;

    home.packages = with pkgs; [
      inotify-tools
      wofi
      swaybg
      # Needed for flameshot
      grim
      slurp
      playerctl
      (rofimoji.override { rofi = my_rofi; })
      fusuma
      swaynotificationcenter

      gammastep
      (import ./tablet_mode_switch { inherit pkgs; })
      squeekboard
      rot8
    ];

    services.kanshi = {
      enable = true;
      systemdTarget = "hyprland-session.target";
      profiles =
        let
          externalScale = 1.25;
          verticalScale = 1.0;
          builtinScale = 1.2;
        in
        {
          undocked = {
            outputs = [
              {
                criteria = "eDP-1";
                scale = builtinScale;
              }
            ];
          };
          docked = {
            outputs = with builtins; [
              {
                criteria = "eDP-1";
                position = "${toString (ceil (960 / externalScale))},${toString (ceil (1600 / externalScale))}";
                scale = builtinScale;
              }
              {
                criteria = "Acer Technologies Acer XR382CQK 0x0000B7AA";
                position = "0,0";
                scale = externalScale;
              }
            ];
          };
          desktop = {
            outputs = with builtins; [
              {
                criteria = "Acer Technologies Acer XR382CQK 0x9227A1AA";
                # note that leaving a small gap here will keep the mouse trapped if moving slow, and
                # require speedy movement to get to the other monitor. could come in handy!
                position = "${toString (ceil (1080 / verticalScale))},0";
                scale = externalScale;
              }
              {
                criteria = "HP Inc. HP VH240a 6CM1290957";
                position = "0,0";
                scale = verticalScale;
                transform = "90";
              }
            ];
          };
        };
    };

    services.flameshot = {
      enable = true;
      # settings = {};
    };

    programs.rofi = {
      enable = true;
      package = my_rofi;
      terminal = "${pkgs.kitty}/bin/kitty";
      theme = "arthur";
    };

    programs.wlogout = {
      enable = true;
    };

    # Keeps track of media players so playerctl always acts on the most
    # recently active one.
    services.playerctld.enable = true;

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = cmd.lock;
          before_sleep_cmd = cmd.lock;
        };
        listener = [
          {
            timeout = 240;
            # Locking should come before screen off to prevent FOIC (flash of insecure content)
            on-timeout = cmd.lock;
          }
          {
            timeout = 480;
            on-timeout = builtins.toString cmd.screenOff;
            on-resume = builtins.toString cmd.screenOn;
          }
          # FIXME: re-enable on laptop but not desktop
          # {
          #   timeout = 600;
          #   on-timeout = "systemctl suspend-then-hibernate";
          # }
        ];
      };
    };

    wayland.windowManager.sway.enable = true;
    wayland.windowManager.hyprland.enable = true;

    services.fusuma = {
      enable = true;
      extraPackages = with pkgs; [
        my_rofi
        coreutils-full
        wtype
      ];
      settings = {
        threshold = {
          swipe = 0.1;
        };
        interval = {
          swipe = 0.7;
        };
        swipe = {
          "3" = {
            left = {
              command = "${pkgs.swaynotificationcenter}/bin/swaync-client --open-panel";
            };
            right = {
              command = "${pkgs.swaynotificationcenter}/bin/swaync-client --close-panel";
            };
          };
          "4" = {
            left = {
              command = "${pkgs.sway}/bin/swaymsg workspace next";
            };
            right = {
              command = "${pkgs.sway}/bin/swaymsg workspace prev";
            };
          };
        };
      };
    };
  };
}
