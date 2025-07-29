{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  # bash script to let dbus know about important env variables and
  # propagate them to relevent services run at the end of sway config
  # see
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # note: this is pretty much the same as  /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables
  dbus-sway-environment = pkgs.writeTextFile {
    name = "dbus-sway-environment";
    destination = "/bin/dbus-sway-environment";
    executable = true;

    text = ''
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
      systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
      systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    '';
  };

  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  configure-gtk = pkgs.writeTextFile {
    name = "configure-gtk";
    destination = "/bin/configure-gtk";
    executable = true;
    text =
      let
        schema = pkgs.gsettings-desktop-schemas;
        datadir = "${schema}/share/gsettings-schemas/${schema.name}";
      in
      ''
        export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
        gnome_schema=org.gnome.desktop.interface
      '';
  };

in
{
  environment.systemPackages = [
    dbus-sway-environment
    configure-gtk
  ];
  environment.sessionVariables = {
    # Force electron apps to run in wayland natively. This is required for
    # them to display on scaled monitors without getting blurry.
    # NB: This breaks copying from the 1password app. It should still work
    # in the browser.
    ELECTRON_OZONE_PLATFORM_HINT = "wayland";
  };

  programs.hyprland.enable = true;
  programs.sway.enable = true;
  programs.hyprlock.enable = true;
  programs.nm-applet.enable = true; # GUI WIFI tool for WMs

  services.dbus.enable = true;
  # Location service provider, required for gammastep
  services.geoclue2.enable = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  xdg.portal = {
    enable = true;
    # fix GTK theming in hyprland
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    wlr.enable = true;
  };

  # Enable a keyring service and password UI for non-gnome environments
  # https://discourse.nixos.org/t/login-keyring-did-not-get-unlocked-hyprland/40869/8?u=smona
  services.gnome.gnome-keyring.enable = true;
  programs.seahorse.enable = true;
  security.pam.services.gdm-password.enableGnomeKeyring = true;

  # Auto-mount (and unmount) removable drives
  services.devmon.enable = true;
  services.gvfs.enable = true;
  services.udisks2.enable = true;
}
