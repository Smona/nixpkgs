{ config, lib, pkgs, inputs, ... }:

let
  commonOptions = import ./common.nix { inherit pkgs inputs config; };
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
          xkb_layout = "us(dvorak),us";
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
          # Enable dynamic tiling
          command = "${pkgs.autotiling}/bin/autotiling";
          always = true;
        }
      ] ++ (builtins.map (cmd: { command = cmd; }) commonOptions.execStart)
        ++ (builtins.map (cmd: {
          command = cmd;
          always = true;
        }) commonOptions.execAlways);

      modifier = "Mod4";
      keybindings = let
        modifier = config.wayland.windowManager.sway.config.modifier;
        secondaryMod = "Mod1";
      in lib.mkOptionDefault ({
        # Allow org-mode to use this hotkey
        "${modifier}+Return" = null;
        # "${secondarymod}+h" = "workspace prev";
        # "${secondaryMod}+l" = "workspace next";
      } // (builtins.listToAttrs (builtins.map (hk: {
        name = (if hk.ctrl or false then "Ctrl+" else "")
          + (if hk.secondaryMod or false then "${secondaryMod}+" else "")
          + (if hk.primaryMod or false then "${modifier}+" else "")
          + (if hk.shift or false then "Shift+" else "") + hk.key;
        value = "exec ${hk.command}";
      }) commonOptions.keyBinds)));
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
