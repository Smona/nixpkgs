{
  primaryMonitor,
  cursorThemeName,
  cursorSize,
}:
''
  exec-once=dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
  exec-once=systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
  exec-once=hyprctl setcursor ${cursorThemeName} ${builtins.toString cursorSize}

  monitor=,preferred,auto,1
  monitor=DP-3, preferred, auto, 1.25
  monitor=DP-2, preferred, auto-left, 1, transform, 1
  workspace = 1,monitor:${primaryMonitor}

  misc:disable_hyprland_logo = yes
''
