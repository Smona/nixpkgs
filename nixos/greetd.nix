{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = {
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${lib.getExe pkgs.greetd.tuigreet} -r --remember-session --time";
        };
      };
    };
    environment.etc."greetd/environments".text = ''
      hyprland
    '';
  };
}
