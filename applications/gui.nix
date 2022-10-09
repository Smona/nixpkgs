{ config, lib, pkgs, ... }:

let settings = import ../settings.nix;
in {
  imports = [ ./firefox.nix ./terminal.nix ]
    ++ (lib.lists.optional settings.roles.gaming ./games.nix);

  home.packages = with pkgs;
    [
      # Graphical applications
      # FIXME: system authentication isn't working, preventing use of SSH agent
      # _1password-gui
      discord
      rescuetime
      signal-desktop
      slack
      spotify
      libreoffice-fresh
      vlc
    ] ++ (lib.lists.optional settings.roles.work zoom-us);

  programs.chromium.enable = true;

  systemd.user.services.rescuetime = {
    Unit.Description = "RescueTime time tracker";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      # TODO: don't go through homeDirectory, reference rescuetime directly
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/rescuetime";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
  # systemd.user.services."1password" = {
  #   Unit.Description = "1password manager GUI application.";
  #   Install.WantedBy = [ "graphical-session.target" ];
  #   Service = {
  #     ExecStart = "${settings.homeDirectory}/.nix-profile/bin/1password";
  #     Restart = "on-failure";
  #     RestartSec = 2;
  #   };
  # };

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
