{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.smona.waybar;
  cmd = import ../system-commands { inherit pkgs inputs; };
  waybar = pkgs.waybar.override { withMediaPlayer = true; };
in {
  options.smona.waybar = { enable = lib.mkEnableOption "waybar status bar"; };

  config = lib.mkIf cfg.enable {
    home.packages = [ waybar ];
    systemd.user.services.waybar = {
      Unit.Description = "Status bar for wlroots";
      Install.WantedBy = [ "sway-session.target" "hyprland-session.target" ];
      Service = {
        ExecStart = "${waybar}/bin/waybar";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };

    xdg.configFile.waybar = {
      onChange = "systemctl --user restart waybar";
      target = "waybar/config";
      text = let
        window_config = {
          format = "{title}";
          max-length = 50;
          rewrite = {
            "(.*) — Mozilla Firefox" = " $1";
            "(.*) — Firefox Developer Edition" = " $1";
            "(.*) – Doom Emacs" = " $1";
            "(.*) - vim" = " $1";
            # "(.*) - kitty" = " $1";
          };
        };
        vertical_bar_icons = [ "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
      in builtins.toJSON {
        layer = "top";
        position = "bottom";
        modules-left = [
          "battery"
          "memory"
          "cpu"
          "idle_inhibitor"
          "sway/workspaces"
          "hyprland/workspaces"
          "tray"
        ];
        modules-center = [ "sway/window" "hyprland/window" ];
        modules-right =
          [ "custom/media" "network" "pulseaudio" "backlight" "clock" ];
        cpu = {
          interval = 2;
          format = "{icon} {usage} %";
          format-icons = vertical_bar_icons;
          max-length = 10;
        };
        backlight = {
          format = "{icon} {percent}";
          format-icons = [ "" "" ];
          on-scroll-up = cmd.darker;
          on-scroll-down = cmd.brighter;
          smooth-scrolling-threshold = 4;
        };
        battery = {
          format = "{icon} {capacity}%";
          interval = 2;
          format-icons = [ "" "" "" "" "" ];
          # Hide the battery indicator when it's full.
          format-full = "";
          states = {
            full = 100;
            normal = 99;
            warning = 25;
            critical = 10;
          };
        };
        clock = {
          interval = 60;
          format = "{:%I:%M %p   %a %m/%d}";
          max-length = 25;
        };
        memory = {
          interval = 10;
          format-icons = vertical_bar_icons;
          format = "{icon} {used} GB";
          max-length = 10;
        };
        network = {
          format-wifi = "  {essid} ({signalStrength}%)";
          tooltip-format =
            "{bandwidthDownBytes}D{bandwidthUpBytes}U {ipaddr}@{essid}";
        };
        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };
        pulseaudio = {
          on-scroll-up = cmd.softer;
          on-scroll-down = cmd.louder;
          on-click = cmd.mute;
          on-click-right = cmd.tao;
          scroll-step = 0;
          smooth-scrolling-threshold = 1;
          format = "{icon} {volume}";
          format-bluetooth = "{volume}";
          format-muted = " {volume}";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" ];
          };
        };
        tray = { spacing = 8; };
        "sway/window" = window_config;
        "hyprland/window" = window_config;
        "hyprland/workspaces" = {
          on-scroll-up = "hyprctl dispatch workspace e+1";
          on-scroll-down = "hyprctl dispatch workspace e-1";
          # window-rewrite = {

          # };
        };
        "custom/media" = {
          format = "{icon} {}";
          escape = true;
          return-type = "json";
          max-length = 40;
          on-click = "playerctl play-pause";
          on-click-right = "playerctl stop";
          smooth-scrolling-threshold = 10;
          on-scroll-up = "playerctl next";
          on-scroll-down = "playerctl previous";
          exec = "${waybar}/bin/waybar-mediaplayer.py 2> /dev/null";
        };
      };
    };

    xdg.configFile."waybar.css" = {
      onChange = "systemctl --user restart waybar";
      target = "waybar/style.css";
      source = ./waybar.css;
    };
  };
}
