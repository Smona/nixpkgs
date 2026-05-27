{
  config,
  lib,
  pkgs,
  ...
}:

let
  my_rofi = pkgs.rofi.override { plugins = with pkgs; [ rofi-calc ]; };
in
{
  imports = [
    ./eww.nix
  ];

  config = lib.mkIf (config.smona.desktop.shell == "custom") {
    home.packages = with pkgs; [
      gammastep
      swaynotificationcenter
      swaybg
    ];

    smona.wlroots = {
      execStart = [
        "gammastep-indicator -t 6500K:3200K -b 1.0:0.8"
        "swaync"
      ];
      # TODO: initialize wallpaper file if it doesn't exist
      # TODO: try using hyprpaper, test on sway
      execAlways = [
        "swaybg -i ~/.config/wallpaper -m fill"
      ];
      sessionMenuCommand = [ "wlogout" ];
      launcherCommand = [
        "rofi"
        "-show"
        "combi"
        "-combi-modes"
        "drun,ssh,run"
        "-show-icons"
      ];
      notificationsCommand = [
        "swaync-client"
        "-t"
      ];
      keyBinds = [
        {
          secondaryMod = true;
          key = "tab";
          command = [
            "rofi"
            "-show"
            "window"
            "-show-icons"
          ];
        }
      ];
    };

    programs.wlogout = {
      enable = true;
    };

    # smona.eww.enable = true;
    smona.waybar.enable = true;

    services.swaync = {
      enable = true;
    };
    # TODO: these are NixOS options, not home-manager. Move into a
    # NixOS-side counterpart gated on smona.desktop.shell == "custom".
    # programs.nm-applet.enable = true; # GUI WIFI tool for WMs
    # services.blueman.enable = true;
    programs.hyprlock.enable = true;

    programs.rofi = {
      enable = true;
      package = my_rofi;
      terminal = "${pkgs.kitty}/bin/kitty";
      theme = ./rofi-theme.rasi;
    };
    catppuccin.rofi.enable = false;

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
