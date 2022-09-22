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
    quake-mode
  ];
  settings = import ../../settings.nix;
in {
  imports = [ ../gui.nix settings.desktops.gnome.theme ];

  home.packages = extensions;

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
      # Allow over-amplification
      "org/gnome/desktop/sound" = { allow-volume-above-100-percent = true; };
      "org/gnome/desktop/wm/preferences" = {
        # Focus windows on hover, but preserve focus when hovering the desktop.
        # Originally switched to fix https://gitlab.gnome.org/GNOME/gnome-shell/-/issues/5162,
        # but it seems more efficient anyways.
        focus-mode = "sloppy";
      };
      "org/gnome/mutter" = {
        # Change focus immediately rather than waiting for pointer to rest
        focus-change-on-pointer-rest = false;
      };
      "com/github/repsac-by/quake-mode" = {
        quake-mode-app = "Alacritty.desktop";
        quake-mode-hide-from-overview = true;
        quake-mode-hotkey = [ "<Alt>space" ];
      };

      # Custom keybindings
      "org/gnome/desktop/wm/keybindings" = {
        # Conflicts with desired guake hotkey
        activate-window-menu = [ "<Shift><Alt>space" ];
        close = [ "<Super><Shift>q" ];
      };
      "org/gnome/settings-daemon/plugins/media-keys" = {
        custom-keybindings = [
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
        ];
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" =
        {
          name = "Toggle guake";
          binding = "<Alt>space";
          command = "guake -t";
        };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" =
        {
          name = "1password Quick Access";
          binding = "<Primary><Shift>space";
          command = "1password --quick-access";
        };
    };
  };
}
