{
  config,
  lib,
  pkgs,
  ...
}:

let
  backgroundImage = ./background;
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
              ${import ../desktops/wlroots/hyprland_common_config.nix}

              # regreet-specific
              windowrule=monitor DP-3, .*
              monitor=DP-2, disable
              exec-once = ${lib.getExe pkgs.greetd.regreet}; hyprctl dispatch exit
            '';
          in
          {
            command = "${lib.getExe pkgs.hyprland} --config ${hyprConfig}";
            # command = "${pkgs.sway}/bin/sway --config ${swayConfig}";
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
          path = backgroundImage;
          fit = "Cover";
        };
      };
    };
  };
}
