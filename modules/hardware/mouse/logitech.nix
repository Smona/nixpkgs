# logiops userland driver + my MX Master 3 settings.
{ ... }:
{
  flake.homeModules.logitech =
    { pkgs, ... }:
    {
      systemd.user.services."logiops" = {
        Unit.Description = "Unofficial userland driver for logitech devices";
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          ExecStart = "${pkgs.logiops_0_2_3}/bin/logid -c ${./_logid.cfg}";
          Restart = "on-failure";
          RestartSec = 2;
        };
      };
    };

  flake.darwinModules.logitech =
    { ... }:
    {
      homebrew.casks = [ "logitech-options" ];
    };
}
