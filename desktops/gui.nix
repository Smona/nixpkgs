{ config, lib, pkgs, ... }:

let settings = import ../settings.nix;
in {
  imports = [ ../applications/firefox.nix ];

  home.packages = with pkgs;
    [
      # Graphical applications
      _1password-gui
      discord
      rescuetime
      signal-desktop
      slack
      spotify
    ] ++ (lib.lists.optional settings.roles.work zoom-us);

  programs.chromium.enable = true;

  systemd.user.services.rescuetime = {
    Unit.Description = "RescueTime time tracker";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/rescuetime";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
  systemd.user.services."1password" = {
    Unit.Description = "1password manager GUI application.";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/1password";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };

  # I like to have slack installed everywhere, but only auto-start it on work machines
  systemd.user.services.slack = lib.mkIf settings.roles.work {
    Unit.Description = "Slack desktop application";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/slack";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
}
