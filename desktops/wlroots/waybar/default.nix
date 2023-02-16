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
      text = ''
        {
              "layer": "top",
              "position": "bottom",
              "modules-left": [ "battery", "memory", "cpu", "idle_inhibitor", "sway/workspaces", "tray" ],
              "modules-center": [ "sway/window", "custom/window-title"],
              "modules-right": [ "custom/media", "network", "pulseaudio", "backlight", "clock" ],
              "cpu": {
                "interval": 2,
                "format": "{icon} {usage} %",
                "format-icons": ["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"],
                "max-length": 10
              },
              "backlight": {
                "format": "{icon} {percent}",
                "format-icons": [ "", "" ],
                "on-scroll-up": "${cmd.darker}",
                "on-scroll-down": "${cmd.brighter}",
                "smooth-scrolling-threshold": 4
              },
              "battery": {
                "format": "{icon} {capacity}%",
                "format-icons": [ "", "", "", "", "" ]
              },
              "clock": {
                "interval": 60,
                "format": "{:%H:%M}",
                "max-length": 25
              },
              "memory": {
                "interval": 10,
                "format-icons": ["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"],
                "format": "{icon} {used} GB",
                "max-length": 10
              },
              "network": {
                "format-wifi": "  {essid} ({signalStrength}%)",
                "tooltip-format": "{bandwidthDownBytes}D{bandwidthUpBytes}U {ipaddr}@{essid}"
              },
              "idle_inhibitor": {
                  "format": "{icon}",
                  "format-icons": {
                      "activated": "",
                      "deactivated": ""
                  }
              },
              "pulseaudio": {
                "on-scroll-up": "${cmd.softer}",
                "on-scroll-down": "${cmd.louder}",
                "on-click": "${cmd.mute}",
                "on-click-right": "${cmd.tao}",
                "scroll-step": 0,
                "smooth-scrolling-threshold": 1,
                "format": "{icon} {volume}",
                "format-bluetooth": "{volume}",
                "format-muted": " {volume}",
                "format-icons": {
                    "headphone": "",
                    "hands-free": "",
                    "headset": "",
                    "phone": "",
                    "portable": "",
                    "car": "",
                    "default": ["", ""]
                }
              },
              "tray": { "spacing": 8 },
              "sway/window": {
                  "format": "{title}",
                  "max-length": 50,
                  "rewrite": {
                    "(.*) — Mozilla Firefox": " $1",
                    "(.*) – Doom Emacs": " $1",
                    "(.*) - vim": " $1",
                    "(.*) - kitty": " [$1]"
                  }
              },
              "custom/media": {
                  "format": "{icon} {}",
                  "escape": true,
                  "return-type": "json",
                  "max-length": 40,
                  "on-click": "playerctl play-pause",
                  "on-click-right": "playerctl stop",
                  "smooth-scrolling-threshold": 10,
                  "on-scroll-up": "playerctl next",
                  "on-scroll-down": "playerctl previous",
                  "exec": "${waybar}/bin/waybar-mediaplayer.py 2> /dev/null",
              },
              "custom/window-title": {
                "interval": 1,
                "return-type": "json",
                "max-length": 50,
                "exec": "${
                  pkgs.writeShellApplication {
                    name = "hypr-get-title";
                    runtimeInputs = with pkgs; [
                      inputs.hyprland.packages.x86_64-linux.hyprland
                      gnused
                      gnugrep
                    ];
                    text = ''
                      TITLE=$(hyprctl activewindow \
                        | grep 'title:' | sed 's/title: //' \
                        | sed 's/</\&lt;/g' | sed 's/>/\&gt;/g' \
                        | sed 's/\(.*\) — Mozilla Firefox$/ \1/' \
                        | sed 's/\(.*\) – Doom Emacs$/ \1/' \
                        | sed 's/\(.*\)zsh$/ \1zsh/')

                      echo "{\"text\": \"$TITLE\", \"class\": \"window-title\" }"
                    '';

                  }
                }/bin/hypr-get-title"
              }
        }
      '';
    };

    xdg.configFile."waybar.css" = {
      onChange = "hyprctl reload";
      target = "waybar/style.css";
      source = ./waybar.css;
    };
  };
}
