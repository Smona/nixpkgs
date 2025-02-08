{
  primaryMonitor,
  cursorThemeName,
  cursorSize,
}:
let
  externalScale = 1.25;
  builtinScale = 1.2;
in
''
  exec-once=dbus-update-activation-environment --systemd --all
  exec-once=systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
  exec-once=hyprctl setcursor ${cursorThemeName} ${toString cursorSize}

  monitor=,preferred,auto,1                           # auto-configure new monitors
  monitor=Unknown-1, disable                          # Disable ghost dock monitor
  monitor=DP-3, preferred, 0x0, ${toString externalScale}, bitdepth, 10                 # Main monitor
  monitor=DP-1, preferred, 0x0, ${toString externalScale}, bitdepth, 10                 # Main monitor
  monitor=DP-2, preferred, auto-left, 1, transform, 1, bitdepth, 10 # Vertical monitor
  monitor=HDMI-A-2, preferred, auto-left, 1, transform, 1, bitdepth, 10 # Vertical monitor
  monitor=eDP-1, preferred, ${toString (3840 / externalScale / 2 - 1920 / builtinScale / 2)}x${
    toString (1600 / externalScale)
  }, ${toString builtinScale}

  workspace = 1,monitor:${primaryMonitor}

  xwayland:force_zero_scaling = yes

  misc:disable_hyprland_logo = yes
''
