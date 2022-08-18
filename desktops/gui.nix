{ config, lib, pkgs, ... }:

let settings = import ../settings.nix;
in {
  home.packages = with pkgs; [
    # Graphical applications
    _1password-gui
    discord
    rescuetime
    signal-desktop
    slack
    spotify
  ];

  programs.firefox.enable = true;
  programs.chromium.enable = true;

  home.sessionVariables = {
    # Enable smooth scrolling and zooming in firefox.
    # Source: https://www.reddit.com/r/firefox/comments/l5a9ez/comment/gktzijc/
    MOZ_ENABLE_WAYLAND = 1;
    MOZ_USE_XINPUT2 = 1;
  };

  systemd.user.services.rescuetime = {
    Unit.Description = "RescueTime time tracker";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/rescuetime";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
  systemd.user.services.slack = {
    Unit.Description = "Slack desktop application";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/slack";
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
}
