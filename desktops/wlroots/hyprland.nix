{ config, lib, pkgs, inputs, ... }:

let commonOptions = import ./common.nix { inherit pkgs inputs config; };
in {
  wayland.windowManager.hyprland = {
    extraConfig = let mod = "SUPER";
    in ''
      monitor=,preferred,auto,1

      exec-once=dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
      exec-once=systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
      # exec-once=1password --silent
      ${(builtins.concatStringsSep "\n"
        (builtins.map (cmd: "exec-once=${cmd}") commonOptions.execStart))}
      ${(builtins.concatStringsSep "\n"
        (builtins.map (cmd: "exec=${cmd}") commonOptions.execAlways))}

      bind=SUPER,C,killactive,
      bind=SUPER,M,exit,

      input {
          kb_file=
          kb_layout=us
          kb_variant=dvorak
          kb_model=
          kb_options=${builtins.concatStringsSep "," commonOptions.xkbOptions}
          kb_rules=
          repeat_rate = 20
          repeat_delay = 400

          follow_mouse=1
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
          main_mod=SUPER

          gaps_in=5
          gaps_out=5
          border_size=2
          col.active_border=0x888130d9
          col.inactive_border=0x00333333

          apply_sens_to_raw=0 # whether to apply the sensitivity to raw input (e.g. used by games where you aim using your mouse)
      }

      decoration {
          rounding=10
          blur=yes
          blur_size=5 # minimum 1
          blur_passes=2 # minimum 1
          blur_new_optimizations=1

          drop_shadow = yes
          shadow_range = 4
          shadow_render_power = 3
          col.shadow = 0xee1a1a1a
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
          no_gaps_when_only = no
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
      }

      layerrule = blur,waybar
      layerrule = ignorezero,waybar
      layerrule = blur,rofi
      layerrule = ignorezero,rofi

      windowrule=tile,class:^(Spotify)$
      windowrule=noblur,title:(Quick Access — 1Password)

      # Float windows
      windowrule=float,1Password
      # windowrule=size 20% 20%,title:(Extension: \(fx_cast\))
      # windowrule=center,title:(Extension: \(fx_cast\))
      windowrule=float,title:^(flameshot)$

      # Firefox PIP floating window
      windowrule=float,title:^(Picture-in-Picture)$
      windowrule=pin,title:^(Picture-in-Picture)$
      windowrule=size 22% 20%,title:^(Picture-in-Picture)$
      windowrule=move 78% 77%,title:^(Picture-in-Picture)$

      # some nice mouse binds
      bindm=${mod},mouse:272,movewindow
      bindm=${mod},mouse:273,resizewindow

      # Key binds
      ${(builtins.concatStringsSep "\n" (builtins.map (hk:
        let bindCommand = if hk.repeat or false then "binde" else "bind";
        in "${bindCommand}=${
          builtins.concatStringsSep "_"
          ((lib.lists.optional hk.ctrl or false "CTRL")
            ++ (lib.lists.optional hk.shift or false "SHIFT")
            ++ (lib.lists.optional hk.primaryMod or false mod)
            ++ (lib.lists.optional hk.secondaryMod or false "ALT"))
        },${hk.key},exec,${hk.command}") commonOptions.keyBinds))}
      bind=${mod},V,togglefloating,
      bind=${mod},P,pseudo,
      bind=${mod},A,togglesplit,
      bind=${mod}_SHIFT,Q,killactive,

      bind=${mod},H,movefocus,l
      bind=${mod},L,movefocus,r
      bind=${mod},K,movefocus,u
      bind=${mod},J,movefocus,d

      bind=${mod}_SHIFT,H,movewindow,l
      bind=${mod}_SHIFT,L,movewindow,r
      bind=${mod}_SHIFT,K,movewindow,u
      bind=${mod}_SHIFT,J,movewindow,d

      binde=SUPER,H,resizeactive,-10 0
      binde=SUPER,L,resizeactive,10 0
      binde=SUPER,J,resizeactive,0 10
      binde=SUPER,K,resizeactive,0 -10

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

      bind=${mod}_SHIFT,1,movetoworkspace,1
      bind=${mod}_SHIFT,2,movetoworkspace,2
      bind=${mod}_SHIFT,3,movetoworkspace,3
      bind=${mod}_SHIFT,4,movetoworkspace,4
      bind=${mod}_SHIFT,5,movetoworkspace,5
      bind=${mod}_SHIFT,6,movetoworkspace,6
      bind=${mod}_SHIFT,7,movetoworkspace,7
      bind=${mod}_SHIFT,8,movetoworkspace,8
      bind=${mod}_SHIFT,9,movetoworkspace,9
      bind=${mod}_SHIFT,0,movetoworkspace,10

      bind=SUPER,mouse_down,workspace,e+1
      bind=SUPER,mouse_up,workspace,e-1
    '';
  };
}
