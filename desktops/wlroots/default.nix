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

  options.smona.wlroots = with lib; {
    enable = mkEnableOption "wlroots window managers";
    builtInDisplay = mkOption {
      description = "Which monitor ID represents the builtin screen. Get the ID via `swaymsg -t get_outputs`";
      type = types.str;
      default = "";
    };
    primaryMonitor = mkOption {
      description = "Which monitor ID represents the 'primary' monitor.";
      type = types.str;
    };
    wallpaper = mkOption {
      description = "Image to use as the desktop wallpaper.";
      type = types.path;
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
          before_sleep_cmd = "${cmd.lock} --immediate";
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
