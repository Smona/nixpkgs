# Configs shared between graphical linux installs and darwin

{
  config,
  lib,
  pkgs,
  ...
}:

let
  my-slack = (config.lib.nixGL.wrap pkgs.slack);
in
{
  imports = [
    ./firefox.nix
    ./terminal.nix
    ./thunderbird
  ];

  options = with lib; {
    graphical = mkEnableOption "installing and configuring graphical applications";

    # High-level toggles for different tasks i want to do on each computer.
    # They're all defined here for ease of discoverability
    roles = {
      art = mkEnableOption "set up computer for visual art creation";
      work = mkEnableOption "set up computer for work";
      music = mkEnableOption "set up computer for music production";
    };
  };

  config = lib.mkIf config.graphical {
    programs.kitty.enable = true;
    # programs.alacritty.enable = true;

    home.packages = with pkgs; [
      # Messaging apps
      vesktop
      my-slack
    ];

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
    # Roon playback target on this computer
    systemd.user.services.squeezelite = {
      Install.WantedBy = [ "default.target" ];
      Service = {
        Type = "exec";
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p .local/share/squeezelite";

        ExecStart = "${pkgs.squeezelite}/bin/squeezelite -s 192.168.0.198 -N .local/share/squeezelite/player-name";
      };
      Unit = {
        Description = "Squeezelite headless streaming music client";
        Wants = [ "sound.target" ];
        After = [ "pipewire-pulse.service" ];
      };
    };
  };
}
