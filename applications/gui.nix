{ config, lib, pkgs, roles, ... }:

{
  imports = [ ./firefox.nix ./terminal.nix ]
    ++ (lib.lists.optional roles.gaming ./games.nix);

  home.packages = with pkgs;
    [
      # Graphical applications
      discord
      rescuetime
      signal-desktop
      slack
      spotify
      libreoffice-fresh
      vlc
    ] ++ (lib.lists.optional roles.work zoom-us);

  programs.chromium.enable = true;

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
  systemd.user.services.slack = lib.mkIf roles.work {
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
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "/usr/bin/env 1password";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
}
