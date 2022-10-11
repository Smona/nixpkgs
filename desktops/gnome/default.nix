{ config, lib, pkgs, ... }:

let
  extensions = with pkgs.gnomeExtensions; [
    # Gnome goodness
    always-indicator
    audio-switcher-40
    autohide-battery
    burn-my-windows
    clipboard-indicator
    # disabled because it doesn't play nice with quake-mode.
    # popup shows up in alt-tab. It's also slower.
    # coverflow-alt-tab
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
  themes = import ./themes.nix;
  settings = import ../../settings.nix;
in {
  imports =
    [ ../../applications/gui.nix themes.${settings.desktops.gnome.theme} ];

  home.packages = extensions ++ [ pkgs.gnome.dconf-editor ];

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
        # Disabled so that I can use audio-switcher-40 on gnome 42
        disable-extension-version-validation = true;
        enabled-extensions = builtins.map (e: e.extensionUuid) extensions;
        favorite-apps = [
          "emacsclient.desktop"
          "firefox.desktop"
          "spotify.desktop"
          "discord.desktop"
          "steam.desktop"
        ];
      };
      "org/gnome/desktop/interface" = { clock-format = "12h"; };
      # Allow over-amplification
      "org/gnome/desktop/sound" = { allow-volume-above-100-percent = true; };
      "org/gnome/desktop/input-sources" = {
        sources = [
          (lib.hm.gvariant.mkTuple [ "xkb" "us+dvorak" ])
          (lib.hm.gvariant.mkTuple [ "xkb" "us" ])
        ];
        xkb-options =
          [ "terminate:ctrl_alt_bksp" "lv3:ralt_switch" "caps:swapescape" ];
      };
      "org/gnome/desktop/peripherals/touchpad" = {
        speed = 0.37;
        tap-to-click = true;
        two-finger-scrolling-enabled = true;
      };
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
      "org/gnome/nautilus/preferences" = {
        # Show image thumbnails for remote file storage
        show-image-thumbnails = "always";
      };

      # Custom keybindings
      "org/gnome/desktop/wm/keybindings" = {
        # Conflicts with desired guake hotkey
        activate-window-menu = [ "<Shift><Alt>space" ];
        close = [ "<Super><Shift>q" ];
      };
      "org/gnome/settings-daemon/plugins/media-keys" = {
        home = [ "<Super>e" ];
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

      # Extensions
      "com/github/repsac-by/quake-mode" = {
        quake-mode-app = "kitty.desktop";
        quake-mode-hide-from-overview = true;
        quake-mode-hotkey = [ "<Alt>space" ];
        quake-mode-tray = false;
      };
      "org/gnome/shell/extensions/system-monitor" = {
        background = "#00000000";
        cpu-show-text = false;
        memory-show-text = false;
        net-show-text = false;
        icon-display = false;
      };
      "org/gnome/shell/extensions/burn-my-windows" = {
        fire-close-effect = false;

        matrix-close-effect = true;
        matrix-open-effect = true;
        matrix-animation-time = 580;
        matrix-overshoot = 0.14;
        matrix-trail-color = "rgb(78,41,244)";
      };
      "org/gnome/shell/extensions/gestureImprovements" = {
        alttab-delay = 100;
        enable-forward-back-gesture = true;
      };
    };
  };
}
