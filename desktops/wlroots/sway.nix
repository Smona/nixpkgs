{ config, lib, pkgs, inputs, ... }:

let
  commonOptions = import ../common.nix;
  cmd = import ./system-commands { inherit pkgs inputs; };
in {
  wayland.windowManager.sway = {
    config = {
      terminal = "kitty";
      # Let's let waybar handle the bars...
      bars = [ ];
      colors = {
        focused = {
          background = "#285577";
          border = "#8130d988";
          childBorder = "#8130d988";
          indicator = "#2e9ef4";
          text = "#ffffff";
        };
      };
      gaps = {
        # Doubling inner gaps gives windows the same space on each side
        inner = 10;
        outer = 5;
        smartBorders = "on";
      };
      input = {
        "*" = { natural_scroll = "enabled"; };
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "dvorak";
          xkb_options = builtins.concatStringsSep "," commonOptions.xkbOptions;
        };
        "type:touchpad" = {
          tap = "enabled";
          click_method = "clickfinger";
          drag = "enabled";
          accel_profile = "adaptive";
          pointer_accel = "0.4";
        };
      };
      focus = {
        # move the mouse to the center of windows when changing focus
        mouseWarping = "container";
        # allow windows to steal focus and switch to their workspace
        newWindow = "focus";
      };
      startup = [
        { command = "dbus-sway-environment"; }
        { command = "configure-gtk"; }
        {
          command = "swaybg -i ${commonOptions.backgroundImage} -m fill";
          always = true;
        }
      ];
      keybindings =
        let modifier = config.wayland.windowManager.sway.config.modifier;
        in lib.mkOptionDefault {
          "${modifier}+space" = "exec rofi -show drun -show-icons";
          "${modifier}+Shift+period" = "exec rofimoji --skin-tone light";
          "Ctrl+Shift+space" = "exec 1password --quick-access";
          Print = "exec flameshot gui";
          XF86MonBrightnessUp = "exec ${cmd.brighter}";
          XF86MonBrightnessDown = "exec ${cmd.darker}";
          XF86AudioLowerVolume = "exec ${cmd.softer}";
          XF86AudioRaiseVolume = "exec ${cmd.louder}";
          XF86AudioMute = "exec ${cmd.mute}";
          XF86AudioPlay = "exec ${cmd.play}";
          # =ALT,XF86AudioLowerVolume,exec,playerctl position 5-
          # =ALT,XF86AudioRaiseVolume,exec,playerctl position 5+
          # "${modifier}+Shift+q" = "kill";
        };
    };
    extraConfig = ''
      ################
      # Window rules #
      ################

      # Firefox PIP floating window
      for_window [title="^Picture-in-Picture$"] floating enable, sticky enable, move position 72 ppt 74 ppt, resize set 27 ppt 25 ppt
    '';
  };
}
