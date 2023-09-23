{ config, lib, pkgs, ... }:

let cfg = config.logitech;
in {
  options.logitech.enabled =
    lib.mkEnableOption "Run logiops logitech settings daemon.";

  config = lib.mkIf cfg.enabled {
    systemd.user.services."logiops" = {
      Unit.Description = "Unofficial userland driver for logitech devices";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${pkgs.logiops}/bin/logid -c ${../dotfiles/logid.cfg}";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };
  };
}
