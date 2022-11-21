{ config, lib, pkgs, inputs, ... }:

let
  commonOptions = import ../common.nix;
  cmd = import ./system-commands { inherit pkgs inputs; };
in {
  wayland.windowManager.sway = {
    config = {
      terminal = "kitty";
      input = {
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "dvorak";
          xkb_options = builtins.concatStringsSep "," commonOptions.xkbOptions;
        };
        "type:touchpad" = {
          tap = "enabled";
          click_method = "clickfinger";
          drag = "enabled";
          natural_scroll = "enabled";
          accel_profile = "adaptive";
          pointer_accel = "0.2";
        };
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
          # "${modifier}+d" =
          #   "exec ${pkgs.dmenu}/bin/dmenu_path | ${pkgs.dmenu}/bin/dmenu | ${pkgs.findutils}/bin/xargs swaymsg exec --";
        };
    };
    extraConfig = ''
      # move the mouse to the center of windows when changing focus
      mouse_warping container
    '';
  };
}
