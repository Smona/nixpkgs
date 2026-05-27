{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  commonOptions = import ./common.nix { inherit pkgs inputs config; };

  # Let dbus know about important env variables and propagate them to
  # relevant services run at the end of sway config. See:
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # Pretty much the same as /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables.
  dbus-sway-environment = pkgs.writeShellScript "dbus-sway-environment" ''
    dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
    systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  '';

  # There is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # The suggested way to set gtk settings is with gsettings; for gsettings
  # to work, we need to tell it where the schemas are using XDG_DATA_DIRS.
  configure-gtk =
    let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in
    pkgs.writeShellScript "configure-gtk" ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      gnome_schema=org.gnome.desktop.interface
    '';
in
{
  config = lib.mkIf (config.smona.desktop.compositor == "sway") {
    wayland.windowManager.sway = {
      enable = true;
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
        "*" = {
          natural_scroll = "enabled";
        };
        "type:keyboard" = {
          xkb_layout = "us(dvorak),us";
          xkb_options = builtins.concatStringsSep "," commonOptions.xkbOptions;
        };
        "type:pointer" = {
          pointer_accel = "0.8";
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
        { command = "${dbus-sway-environment}"; }
        { command = "${configure-gtk}"; }
        {
          # Enable dynamic tiling
          command = "${pkgs.autotiling}/bin/autotiling";
          always = true;
        }
      ]
      ++ (builtins.map (cmd: { command = cmd; }) commonOptions.execStart)
      ++ (builtins.map (cmd: {
        command = cmd;
        always = true;
      }) commonOptions.execAlways);

      modifier = "Mod4";
      keybindings =
        let
          modifier = config.wayland.windowManager.sway.config.modifier;
          secondaryMod = "Mod1";
        in
        lib.mkOptionDefault (
          {
            # Allow org-mode to use this hotkey
            "${modifier}+Return" = null;
            Print = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot --notify savecopy area";
            # "${secondarymod}+h" = "workspace prev";
            # "${secondaryMod}+l" = "workspace next";
          }
          // (builtins.listToAttrs (
            builtins.map (hk: {
              name = (
                builtins.concatStringsSep "+" (
                  (lib.lists.optional hk.ctrl or false "Ctrl")
                  ++ (lib.lists.optional hk.shift or false "Shift")
                  ++ (lib.lists.optional hk.primaryMod or false modifier)
                  ++ (lib.lists.optional hk.secondaryMod or false secondaryMod)
                  ++ [ hk.key ]
                )
              );
              value = "exec ${builtins.concatStringsSep " " hk.command}";
            }) commonOptions.keyBinds
          ))
        );
    };
    extraConfig = ''
      ################
      # Window rules #
      ################

      # Firefox PIP floating window
      for_window [title="^Picture-in-Picture$"] floating enable, sticky enable, move position 72 ppt 74 ppt, resize set 27 ppt 25 ppt

      output HDMI-A-2 pos 3840 0 transform 270
      output HDMI-A-1 pos 3840 0 transform 270
    '';
    };
  };
}
