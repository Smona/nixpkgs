{
  config,
  lib,
  pkgs,
  ...
}:

let
  nixGL = import ./nixGL.nix { inherit pkgs config; };
  my-slack = (nixGL pkgs.slack);
  theme = import ../theme.nix { inherit pkgs; };
in
{
  imports = [
    ./art.nix
    ./firefox.nix
    ./terminal.nix
    ./games.nix
    ./music.nix
  ];

  options.graphical = lib.mkEnableOption "install and configure graphical applications.";
  options._1passwordBinary = lib.mkOption {
    type = lib.types.str;
    default = "/usr/bin/env 1password";
  };
  options.nixGLPrefix = lib.mkOption {
    type = lib.types.str;
    default = "";
    description = ''
      Will be prepended to commands which require working OpenGL.

      This needs to be set to the right nixGL package on non-NixOS systems.
    '';
  };
  # Roles are all defined here for ease of discoverability
  options.roles = {
    art = lib.mkEnableOption "set up computer for visual art creation";
    gaming = lib.mkEnableOption "set up computer for gaming";
    work = lib.mkEnableOption "set up computer for work";
    music = lib.mkEnableOption "set up computer for music production";
  };

  config = lib.mkIf config.graphical {
    home.sessionVariables = {
      # Automatically set ozone platform flags in all the places where it matters
      NIXOS_OZONE_WL = 1;
    };

    # Graphical applications
    home.packages =
      with pkgs;
      [
        theme.uiFont.package
        dconf-editor

        gparted
        gthumb
        shotwell
        file-roller
        (nixGL keybase-gui)
        gcolor3

        # Messaging apps
        (nixGL (
          discord.override {
            nss = nss_latest; # https://github.com/NixOS/nixpkgs/issues/78961
          }
        ))
        (nixGL signal-desktop)
        my-slack

        # Media apps
        (nixGL spotify)
        (nixGL libreoffice-fresh)
        (nixGL clapper)
        xournalpp
      ]
      ++ (lib.lists.optionals config.roles.work [ (nixGL gimp) ]);

    gtk.enable = true;
    # TODO integrate with greetd & theme file
    gtk.catppuccin.enable = true;
    gtk.catppuccin.icon.enable = true;
    gtk.cursorTheme = (theme.cursor // { size = theme.cursorSize; });
    # gtk.theme = theme.gtk;
    # gtk.iconTheme = theme.icons;

    programs.chromium = {
      enable = true;
      package = (nixGL config.pkgsCompat.chromium);
    };
    programs.firefox.enable = true;
    programs.kitty.enable = true;
    programs.alacritty.enable = true;

    # Required for keybase-gui
    services.kbfs.enable = true;

    # I like to have slack installed everywhere, but only auto-start it on work machines
    systemd.user.services.slack = lib.mkIf config.roles.work {
      Unit.Description = "Slack desktop application";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${my-slack}/bin/slack";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };

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
    xdg.configFile."1password-settings" = {
      target = "1Password/settings/settings.json";
      text = ''
        {
          "app.double": false,
          "security.authenticatedUnlock.enabled": true,
          "sshAgent.enabled": true,
          "sshAgent.storeKeyTitles": true,
          "sshAgent.sshAuthorizatonModel": "application"
        }
      '';
    };

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
          "image/png" = imageViewer;
          "image/jpeg" = imageViewer;
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
