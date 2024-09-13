{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.smona.waybar;
  cmd = import ../system-commands { inherit pkgs inputs; };
  waybar = pkgs.waybar.override { withMediaPlayer = true; };
in
{
  options.smona.waybar = {
    enable = lib.mkEnableOption "waybar status bar";
  };

  config = lib.mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      systemd = {
        enable = true;
        target = "hyprland-session.target";
      };
      settings =
        let
          window_config = {
            format = "{title}";
            max-length = 50;
            # Show title per-monitor
            separate-outputs = true;
            rewrite = {
              "(.*) — Mozilla Firefox" = " $1";
              "(.*) — Firefox Developer Edition" = " $1";
              "(.*) – Doom Emacs" = " $1";
              "(.*) - vim" = " $1";
              # "(.*) - kitty" = " $1";
            };
          };
          vertical_bar_icons = [
            "▁"
            "▂"
            "▃"
            "▄"
            "▅"
            "▆"
            "▇"
            "█"
          ];
        in
        {
          mainBar = {
            layer = "top";
            position = "bottom";
            modules-left = [
              "battery"
              "memory"
              "cpu"
              "idle_inhibitor"
              "sway/workspaces"
              "hyprland/workspaces"
              "sway/window"
              "hyprland/window"
            ];
            # modules-center = [];
            modules-right = [
              "custom/media"
              "tray"
              "pulseaudio"
              "backlight"
              "clock"
            ];
            cpu = {
              interval = 2;
              format = "{icon} {usage} %";
              format-icons = vertical_bar_icons;
              max-length = 10;
            };
            backlight = {
              format = "{icon} {percent}";
              format-icons = [
                ""
                ""
              ];
              on-scroll-up = cmd.darker;
              on-scroll-down = cmd.brighter;
              smooth-scrolling-threshold = 4;
            };
            battery = {
              format = "{icon} {capacity}%";
              interval = 2;
              format-icons = [
                ""
                ""
                ""
                ""
                ""
              ];
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
              # formatting options: https://fmt.dev/latest/syntax/#chrono-format-specifications
              format = "{:%H:%M   %a %m/%d}";
              max-length = 25;
            };
            memory = {
              interval = 10;
              format-icons = vertical_bar_icons;
              format = "{icon} {used} GB";
              max-length = 10;
            };
            network = {
              format-wifi = " {signalStrength}";
              tooltip-format = "{bandwidthDownBytes}D{bandwidthUpBytes}U {ipaddr}@{essid}";
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
                default = [
                  ""
                  ""
                ];
              };
            };
            tray = {
              spacing = 8;
            };
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
      style = ''
        * {
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: FontAwesome, Roboto, Helvetica, Arial, sans-serif;
          font-size: 15px;
        }

        window#waybar {
          background-color: alpha(@crust, 0.85);
          /* border-top: 3px solid rgba(100, 114, 125, 0.5); */
          color: @text;
          transition-property: background-color;
          transition-duration: 0.5s;
        }

        window#waybar.hidden {
          opacity: 0.2;
        }

        /*
        window#waybar.empty {
            background-color: transparent;
        }
        window#waybar.solo {
            background-color: #FFFFFF;
        }
        */

        window#waybar.termite {
          background-color: #3f3f3f;
        }

        window#waybar.chromium {
          background-color: #000000;
          border: none;
        }

        #workspaces button {
          padding: 0 11px;
          background-color: transparent;
          color: @text;
          /* Use box-shadow instead of border so the text isn't offset */
          box-shadow: inset 0 -3px transparent;
          /* Avoid rounded borders under each workspace name */
          border: none;
          border-radius: 0;
        }

        /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
        #workspaces button:hover {
          background: @surface0;
        }

        #workspaces button.focused,
        #workspaces button.active {
          background-color: @surface1;
          box-shadow: inset 0 -3px @text;
        }

        #workspaces button.urgent {
          background-color: #5e29ff;
        }

        #mode {
          background-color: #64727d;
          border-bottom: 3px solid #ffffff;
        }

        #clock,
        #battery,
        #cpu,
        #memory,
        #disk,
        #temperature,
        #backlight,
        #network,
        #pulseaudio,
        #custom-media,
        #tray,
        #mode,
        #idle_inhibitor,
        #mpd {
          padding: 2px 10px;
          color: @text;
        }

        #window,
        #workspaces {
          margin: 0 4px;
        }

        /* If workspaces is the leftmost module, omit left margin */
        .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
        }

        /* If workspaces is the rightmost module, omit right margin */
        .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
        }

        #clock {
          /* background-color: #64727d; */
        }

        #battery {
          color: @text;
        }

        #battery.warning {
          color: #f5a13b;
        }

        #battery.charging,
        #battery.plugged {
          color: @green;
          background: transparent;
          animation-name: pulse;
          animation-duration: 2s;
          animation-timing-function: ease-in;
          animation-iteration-count: infinite;
          animation-direction: alternate;
        }

        @keyframes pulse {
          to {
            color: alpha(@green, 0.7);
          }
        }

        @keyframes blink {
          0% {
            background-color: transparent;
            color: #f2304e;
          }
          80% {
            background-color: transparent;
            color: #f2304e;
          }
          100% {
            color: #f00;
          }
        }

        #battery.critical:not(.charging) {
          animation-name: blink;
          animation-duration: 2s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
        }

        label:focus {
          background-color: #000000;
        }

        #cpu {
          /* background-color: #2ecc71; */
          color: @blue;
        }

        #memory {
          color: @mauve;
        }

        #disk {
          background-color: #964b00;
        }

        #backlight {
          /* background-color: #90b1b1; */
        }

        #network {
          /* background-color: #2980b9; */
        }

        #network.disconnected {
          background-color: #f53c3c;
        }

        #pulseaudio.muted {
          background-color: transparent;
          color: @overlay2;
        }

        #custom-media {
          color: @sapphire;
          min-width: 100px;
        }

        #custom-media.custom-spotify {
          color: @green;
        }

        #custom-media.custom-vlc {
          color: #ffa000;
        }

        #temperature {
          background-color: #f0932b;
        }

        #temperature.critical {
          background-color: #eb4d4b;
        }

        #tray {
        }

        #tray > .passive {
          -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
        }

        #idle_inhibitor {
          background-color: #2d3436;
        }

        #idle_inhibitor.activated {
          background-color: #ecf0f1;
          color: #2d3436;
        }

        #mpd {
          background-color: #66cc99;
          color: #2a5c45;
        }

        #mpd.disconnected {
          background-color: #f53c3c;
        }

        #mpd.stopped {
          background-color: #90b1b1;
        }

        #mpd.paused {
          background-color: #51a37a;
        }

        #language {
          background: #00b093;
          color: #740864;
          padding: 0 5px;
          margin: 0 5px;
          min-width: 16px;
        }

        #keyboard-state {
          background: #97e1ad;
          color: #000000;
          padding: 0 0px;
          margin: 0 5px;
          min-width: 16px;
        }

        #keyboard-state > label {
          padding: 0 5px;
        }

        #keyboard-state > label.locked {
          background: rgba(0, 0, 0, 0.2);
        }

        .window-title {
          font-weight: bold;
        }
      '';
    };
  };
}
