{ config, lib, pkgs, ... }:

let
  extensions = with pkgs.gnomeExtensions; [
    # Gnome goodness
    always-indicator
    audio-switcher-40
    autohide-battery
    burn-my-windows
    clipboard-indicator
    coverflow-alt-tab
    dash-to-dock
    desktop-icons-ng-ding
    espresso # Requires gnome 40 (Ubuntu >= jammy jellyfish)
    gesture-improvements
    hide-keyboard-layout
    middle-click-to-close-in-overview
    noannoyance-2 # TODO: settings dialog broken
    openweather
    pip-on-top # Fix firefox PIP pinning on wayland
    remove-alttab-delay-v2
    system-monitor # Requires Ubuntu > 20.04
    unite
    user-themes
    windownavigator
  ];
  settings = import ../../settings.nix;
in {
  imports = [ ../gui.nix settings.desktops.gnome.theme ];

  gtk.enable = true;
  gtk.cursorTheme = {
    package = pkgs.dracula-theme;
    name = "Dracula-cursors";
  };

  dconf = {
    enable = true;
    settings = {
      "org/gnome/shell" = {
        disable-user-extensions = false;
        enabled-extensions = builtins.map (e: e.extensionUuid) extensions;
        favorite-apps =
          [ "emacsclient.desktop" "firefox.desktop" "spotify.desktop" ];
      };
    };
  };

  home.packages = extensions;
}
