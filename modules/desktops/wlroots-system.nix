# System-level settings for wlroots compositors (Hyprland, Niri, Sway)
{ ... }:

{
  flake.nixosModules.wlroots-system =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      inWlroots = builtins.elem config.smona.desktop.compositor [
        "hyprland"
        "niri"
        "sway"
      ];
    in
    {
      config = lib.mkIf inWlroots {
        environment.sessionVariables = {
          # Force electron apps to run in wayland natively. This is required for
          # them to display on scaled monitors without getting blurry.
          # NB: This breaks copying from the 1password app. It should still work
          # in the browser.
          ELECTRON_OZONE_PLATFORM_HINT = "wayland";
        };

        services.dbus.enable = true;
        # Location service provider, required for gammastep
        services.geoclue2.enable = true;

        hardware.bluetooth.enable = true;

        xdg.portal = {
          enable = true;
          # fix GTK theming in hyprland
          extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
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
      };
    };
}
