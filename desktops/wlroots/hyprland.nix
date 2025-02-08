{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  commonOptions = import ./common.nix { inherit pkgs inputs config; };
  cfg = config.smona.wlroots;
in
{
  config = lib.mkIf cfg.enable {
    services.hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        splash = false;
      };
    };
    # Add item to nautilus context menu to set the desktop bg dynamically.
    home.file.".local/share/nautilus/scripts/Set as Desktop Background" = {
      text = ''
        #!/usr/bin/env bash
        hyprctl hyprpaper reload $(echo ",$NAUTILUS_SCRIPT_SELECTED_URIS" | sed 's/file:\/\///') > ~/desktop_background_script_result.txt
      '';
      executable = true;
    };

    programs.hyprlock = {
      enable = true;
      extraConfig = ''
          label {
            monitor = ${cfg.primaryMonitor}
            text = $TIME
            text_align = center
            color = rgba(200, 200, 200, 1.0)
            font_size = 40
            font_family = Noto Sans
            rotate = 0 # degrees, counter-clockwise

            position = 0, 80
            halign = center
            valign = center
        }
      '';
      settings = {
        general = {
          grace = 5;
          hide_cursor = false;
        };
        background = {
          color = "rgba(25, 20, 50, 1.0)";
          # path = builtins.toString cfg.wallpaper;
          # TODO: try again after i switch off novideo
          path = "screenshot";
          blur_passes = 5;
          blur_size = 1;
        };
        input-field = [
          {
            size = "200, 50";
            position = "0, -80";
            monitor = cfg.primaryMonitor;
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            font_family = "Noto Sans";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(95, 100, 124)";
            # outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            placeholder_text = ''
              <span foreground="##cad3f5">Password...</span>
            '';
            shadow_passes = 2;
          }
        ];
      };
    };

    wayland.windowManager.hyprland = {
      extraConfig =
        let
          mod = "SUPER";
          left = "d";
          down = "h";
          up = "t";
          right = "n";
        in
        ''
          ${import ./hyprland_common_config.nix {
            primaryMonitor = cfg.primaryMonitor;
            cursorThemeName = config.gtk.cursorTheme.name;
            cursorSize = config.gtk.cursorTheme.size;
          }}

          # TODO: try starting in greetd again, config path was wrong before
          exec-once=${pkgs.openrgb}/bin/openrgb --startminimized -p ~/.config/OpenRGB/Default.orp
          ${(builtins.concatStringsSep "\n" (builtins.map (cmd: "exec-once=${cmd}") commonOptions.execStart))}
          ${(builtins.concatStringsSep "\n" (builtins.map (cmd: "exec=${cmd}") commonOptions.execAlways))}

          bind=SUPER,C,killactive,

          input {
              kb_file=
              kb_layout=us,us
              kb_variant=dvorak,
              kb_options=grp:win_space_toggle
              kb_model=
              kb_options=${builtins.concatStringsSep "," commonOptions.xkbOptions}
              kb_rules=
              repeat_rate = 20
              repeat_delay = 400

              follow_mouse=1
              scroll_factor=0.7
              natural_scroll=yes

              touchpad {
                  natural_scroll=yes
                  # press down 2 fingers for right click, 3 for middle
                  # https://wayland.freedesktop.org/libinput/doc/latest/clickpad-softbuttons.html#clickfinger-behavior
                  clickfinger_behavior=yes
              }

              accel_profile = adaptive
              sensitivity = 0.6
          }

          general {
              gaps_in=8
              gaps_out=12
              border_size=2
              resize_on_border = yes
              col.active_border=rgba($lavenderAlphacc)
              col.inactive_border=rgba($overlay0Alphaaa)
          }

          decoration {
              rounding=12
              blur {
                passes=2 # minimum 1
                size=5 # minimum 1
              }
          }

          animations {
              enabled=1
              animation=windows,1,7,default
              animation=border,1,10,default
              animation=fade,1,10,default
              animation=workspaces,1,6,default
          }

          dwindle {
              pseudotile=0 # enable pseudotiling on dwindle
              preserve_split = yes
          }

          gestures {
              workspace_swipe=yes
          }

          misc {
            # Allow windows to steal focus
            focus_on_activate = yes
            enable_swallow = yes
            swallow_regex = (kitty)
            # Sorry, hypr-chan...
            disable_hyprland_logo = yes
          }

          layerrule = blur,waybar
          layerrule = ignorezero,waybar
          layerrule = blur,rofi
          layerrule = ignorezero,rofi
          layerrule = blur,swaync-control-center

          windowrule=tile,class:^(Spotify)$
          windowrulev2=stayfocused,title:(Quick Access — 1Password)
          windowrulev2=pin,class:^(vlc)$

          # Float windows
          windowrule=float,1Password
          windowrule=float,class:xdg-desktop-portal-gtk
          # windowrule=size 20% 20%,title:(Extension: \(fx_cast\))
          # windowrule=center,title:(Extension: \(fx_cast\))
          windowrule=float,title:^(flameshot)$
          windowrule=float,title:^(doom-capture)$

          # Firefox PIP floating window
          windowrule=float,title:^(Picture-in-Picture)$
          windowrule=pin,title:^(Picture-in-Picture)$
          windowrule=size 22% 20%,title:^(Picture-in-Picture)$
          windowrule=move 78% 77%,title:^(Picture-in-Picture)$

          # some nice mouse binds
          bindm=${mod},mouse:272,movewindow
          bindm=${mod},mouse:273,resizewindow

          # Key binds
          ${
            (builtins.concatStringsSep "\n" (
              builtins.map (
                hk:
                let
                  bindCommand = "bind${if hk.repeat or false then "e" else ""}${
                    if hk.allow_while_locked or false then "l" else ""
                  }";
                in
                "${bindCommand}=${
                  builtins.concatStringsSep "_" (
                    (lib.lists.optional hk.ctrl or false "CTRL")
                    ++ (lib.lists.optional hk.shift or false "SHIFT")
                    ++ (lib.lists.optional hk.primaryMod or false mod)
                    ++ (lib.lists.optional hk.secondaryMod or false "ALT")
                  )
                },${hk.key},exec,${hk.command}"
              ) commonOptions.keyBinds
            ))
          }
          bind=${mod},V,togglefloating,
          bind=${mod},F,fullscreen
          bind=${mod},P,pseudo,
          bind=${mod},S,togglesplit,
          bind=${mod}_SHIFT,Q,killactive,
          bind=,Print,exec,${pkgs.grimblast}/bin/grimblast --notify copysave area

          bind=Control_L&Alt_L,C,exec,~/.emacs.d/bin/org-capture

          bind=${mod},${left},movefocus,l
          bind=${mod},${right},movefocus,r
          bind=${mod},${up},movefocus,u
          bind=${mod},${down},movefocus,d

          bind=${mod}_Alt_L,${left},movewindow,l
          bind=${mod}_Alt_L,${right},movewindow,r
          bind=${mod}_Alt_L,${up},movewindow,u
          bind=${mod}_Alt_L,${down},movewindow,d

          binde=Control_L&Alt_L,${left},resizeactive,-10 0
          binde=Control_L&Alt_L,${right},resizeactive,10 0
          binde=Control_L&Alt_L,${down},resizeactive,0 10
          binde=Control_L&Alt_L,${up},resizeactive,0 -10

          bind=${mod},1,workspace,1
          bind=${mod},2,workspace,2
          bind=${mod},3,workspace,3
          bind=${mod},4,workspace,4
          bind=${mod},5,workspace,5
          bind=${mod},6,workspace,6
          bind=${mod},7,workspace,7
          bind=${mod},8,workspace,8
          bind=${mod},9,workspace,9
          bind=${mod},0,workspace,10

          bind=${mod}_Alt_L,1,movetoworkspace,1
          bind=${mod}_Alt_L,2,movetoworkspace,2
          bind=${mod}_Alt_L,3,movetoworkspace,3
          bind=${mod}_Alt_L,4,movetoworkspace,4
          bind=${mod}_Alt_L,5,movetoworkspace,5
          bind=${mod}_Alt_L,6,movetoworkspace,6
          bind=${mod}_Alt_L,7,movetoworkspace,7
          bind=${mod}_Alt_L,8,movetoworkspace,8
          bind=${mod}_Alt_L,9,movetoworkspace,9
          bind=${mod}_Alt_L,0,movetoworkspace,10

          bind=SUPER,mouse_down,workspace,e+1
          bind=SUPER,mouse_up,workspace,e-1
        '';
    };
  };
}
