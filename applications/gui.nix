# Configs specific to graphical linux systems

{
  config,
  lib,
  pkgs,
  ...
}:

let
  theme = import ../theme.nix { inherit pkgs; };
in
{
  imports = [
    ./common_gui.nix
    ./art.nix
    ./games.nix
    ./music.nix
  ];

  options._1passwordBinary = lib.mkOption {
    type = lib.types.str;
    default = "/usr/bin/env 1password";
  };

  config = lib.mkIf config.graphical {
    home.sessionVariables = {
      # Automatically set ozone platform flags in all the places where it matters
      NIXOS_OZONE_WL = 1;
    };

    # Graphical applications
    home.packages = with pkgs; [
      theme.uiFont.package
      dconf-editor
      obsidian

      # System utilities
      gparted
      gthumb
      shotwell
      file-roller
      (config.lib.nixGL.wrap keybase-gui)
      gcolor3
      font-manager

      electricsheep

      # Messaging apps not supported on darwin
      (config.lib.nixGL.wrap signal-desktop)

      # Creative apps
      freecad

      # Media apps
      (config.lib.nixGL.wrap libreoffice-fresh)
      (config.lib.nixGL.wrap vlc)
      xournalpp
      plexamp
    ];

    gtk.enable = true;
    # TODO integrate with greetd & theme file
    catppuccin.gtk.icon.enable = true;
    gtk.cursorTheme = (theme.cursor // { size = theme.cursorSize; });
    # de-deprecate catppuccin.gtk.enable 😁
    gtk.theme = {
      name = "catppuccin-mocha-mauve-standard";
      package = pkgs.catppuccin-gtk.override {
        size = "standard";
        accents = [ "mauve" ];
        variant = "mocha";
      };
    };

    # gtk.theme = theme.gtk;
    # gtk.iconTheme = theme.icons;
    qt.enable = true;
    qt.style.name = "kvantum";
    qt.platformTheme.name = "kvantum";

    # TODO: install browsers with nix on darwin, maybe with
    programs.chromium = {
      enable = true;
      package = (config.lib.nixGL.wrap config.pkgsCompat.chromium);
    };
    programs.firefox.enable = true;

    # Required for keybase-gui
    services.kbfs.enable = true;

    # 1password has to be installed at the system level to integrate with polkit
    # and support system authentication / ssh agent / browser extension integration.
    systemd.user.services."1password" = {
      Unit.Description = "1password manager GUI application.";
      Install.WantedBy = [
        "graphical-session.target"
        "hyprland-session.target"
        "sway-session.target"
      ];
      Service = {
        ExecStart = "${config._1passwordBinary} --silent";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };
    # TODO: get this working on darwin
    # FIXME: this is breaking 1password settings currently
    # xdg.configFile."1password-settings" = {
    #   target = "1Password/settings/settings.json";
    #   text = ''
    #     {
    #       "app.double": false,
    #       "security.authenticatedUnlock.enabled": true,
    #       "sshAgent.enabled": true,
    #       "sshAgent.storeKeyTitles": true,
    #       "sshAgent.sshAuthorizatonModel": "application"
    #     }
    #   '';
    # };

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let
          browser = "firefox.desktop";
          imageViewer = "gthumb.desktop";
        in
        {
          "application/pdf" = "com.github.xournalpp.xournalpp.desktop";
          "application/x-gzip" = "org.gnome.FileRoller.desktop";
          "x-scheme-handler/http" = browser;
          "text/html" = browser;
          "application/xhtml+xml" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/mailto" = "thunderbird.desktop";
          "image/png" = imageViewer;
          "image/jpeg" = imageViewer;
          "inode/directory" = "org.gnome.Nautilus.desktop";
        };
      associations.added =
        let
          browsers = [
            "firefox.desktop"
            "chromium-browser.desktop"
          ];
          imageViewers = [
            "org.gnome.eog.desktop"
            "org.gnome.gThumb.desktop"
          ];
        in
        {
          "application/pdf" = [ "com.github.xournalpp.xournalpp.desktop" ];
          "application/x-gzip" = [ "org.gnome.FileRoller.desktop" ];
          "image/jpeg" = imageViewers;
          "image/png" = imageViewers;
          "x-scheme-handler/http" = browsers;
          "text/html" = browsers;
          "application/xhtml+xml" = browsers;
          "x-scheme-handler/https" = browsers;
        };
    };

    dconf = {
      enable = true;
      # Setup global GTK settings
      settings =
        let
          fileChooserPrefs = {
            # Sort folders first in nautilus
            sort-directories-first = true;
          };
        in
        {
          # Don't show a warning when opening dconf-editor
          "ca/desrt/dconf-editor" = {
            show-warning = false;
          };
          "org/gnome/desktop/interface" = {
            color-scheme = "prefer-dark";
          };
          "org/gnome/nautilus/preferences" = {
            # Show image thumbnails for remote file storage
            show-image-thumbnails = "always";
          };
          "org/gtk/settings/debug" = {
            enable-inspector-keybinding = true;
          };
          "org/gtk/settings/file-chooser" = fileChooserPrefs;
          "org/gtk/gtk4/settings/file-chooser" = fileChooserPrefs;
          "org/gnome/desktop/interface" = {
            font-name = "${theme.uiFont.name} ${builtins.toString theme.uiFont.size}";
          };
        };
    };
  };
}
