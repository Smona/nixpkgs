{ config, lib, pkgs, ... }:

{
  imports = [ ./firefox.nix ./terminal.nix ./games.nix ./music.nix ];

  options.graphical =
    lib.mkEnableOption "install and configure graphical applications.";
  options._1passwordBinary = lib.mkOption {
    type = lib.types.str;
    default = "/usr/bin/env 1password";
  };
  options.roles = {
    gaming = lib.mkEnableOption "set up computer for gaming";
    work = lib.mkEnableOption "set up computer for work";
    music = lib.mkEnableOption "set up computer for music production";
  };

  config = lib.mkIf config.graphical {
    # Graphical applications
    home.packages = with pkgs;
      [
        # https://github.com/NixOS/nixpkgs/issues/78961
        (discord.override { nss = nss_latest; })
        gthumb
        rescuetime
        signal-desktop
        # Telegram desktop
        tdesktop
        slack
        spotify
        libreoffice-fresh
        vlc
      ] ++ (lib.lists.optionals config.roles.work [ gimp zoom-us ]);

    programs.chromium.enable = true;
    programs.firefox.enable = true;
    programs.kitty.enable = true;

    systemd.user.services.rescuetime = {
      Unit.Description = "RescueTime time tracker";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${pkgs.rescuetime}/bin/rescuetime";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };

    # I like to have slack installed everywhere, but only auto-start it on work machines
    systemd.user.services.slack = lib.mkIf config.roles.work {
      Unit.Description = "Slack desktop application";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${pkgs.slack}/bin/slack";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };

    # 1password has to be installed at the system level to integrate with polkit
    # and support system authentication / ssh agent / browser extension integration.
    systemd.user.services."1password" = {
      Unit.Description = "1password manager GUI application.";
      Install.WantedBy = [ "graphical-session.target" "sway-session.target" ];
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
      defaultApplications = let
        browser = "firefox.desktop";
        imageViewer = "org.gnome.eog.desktop";
      in {
        "x-scheme-handler/http" = browser;
        "text/html" = browser;
        "application/pdf" = browser;
        "application/xhtml+xml" = browser;
        "x-scheme-handler/https" = browser;
        "image/png" = imageViewer;
        "image/jpeg" = imageViewer;
      };
      associations.added = let
        browsers = [ "firefox.desktop" "chromium-browser.desktop" ];
        imageViewers = [ "org.gnome.eog.desktop" "org.gnome.gThumb.desktop" ];
      in {
        "image/jpeg" = imageViewers;
        "image/png" = imageViewers;
        "x-scheme-handler/http" = browsers;
        "text/html" = browsers;
        "application/xhtml+xml" = browsers;
        "x-scheme-handler/https" = browsers;
      };
    };
  };
}
