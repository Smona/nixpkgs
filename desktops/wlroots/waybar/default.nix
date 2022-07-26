{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.smona.waybar;
  cmd = import ../system-commands { inherit pkgs; };
in {
  options.smona.waybar = { enable = lib.mkEnableOption "waybar status bar"; };

  config = lib.mkIf cfg.enable {
    systemd.user.services.waybar = {
      Unit.Description = "Status bar for wlroots";
      Install.WantedBy = [ "sway-session.target" "hyprland-session.target" ];
      Service = {
        ExecStart = "${pkgs.waybar}/bin/waybar";
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
              "modules-left": [ "battery", "memory", "cpu"  ],
              "modules-center": [ "custom/window-title" ],
              "modules-right": [ "tray", "pulseaudio", "backlight", "clock" ],
              "cpu": {
                "interval": 10,
                "format": " {}%",
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
                "interval": 30,
                "format": " {}%",
                "max-length": 10
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
