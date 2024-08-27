{
  config,
  lib,
  pkgs,
  ...
}:

let
  theme = import ../theme.nix { inherit pkgs; };
in
{
  config = {

    services.greetd = {
      enable = true;
      settings = {
        default_session =
          let
            hyprConfig = pkgs.writeText "regreet-hyprland-config" ''
              ${import ../desktops/wlroots/hyprland_common_config.nix {
                primaryMonitor = config.smona.primaryMonitor;
                cursorThemeName = theme.cursor.name;
                cursorSize = theme.cursorSize;
              }}

              # regreet-specific
              windowrule=monitor ${config.smona.primaryMonitor}, .*
              monitor=DP-2, disable
              exec-once = ${lib.getExe pkgs.greetd.regreet}; hyprctl dispatch exit
            '';
          in
          {
            command = "${lib.getExe pkgs.hyprland} --config ${hyprConfig}";
          };
      };
    };
    environment.etc."greetd/environments".text = ''
      hyprland
    '';
    programs.regreet = {
      enable = true;
      theme = theme.gtk;
      iconTheme = theme.icons;
      font = theme.uiFont;
      cursorTheme = theme.cursor;
      settings = {
        background = {
          path = config.smona.wallpaper;
          fit = "Cover";
        };
      };
    };
  };
}
