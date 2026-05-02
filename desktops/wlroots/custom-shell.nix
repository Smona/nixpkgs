{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./eww.nix
  ];

  home.packages = with pkgs; [
    gammastep
    swaynotificationcenter
    swaybg
  ];

  programs.wlogout = {
    enable = true;
  };

  # smona.eww.enable = true;
  smona.waybar.enable = true;

  services.swaync = {
    enable = true;
  };
  # TODO: both of these are actually nixos options
  programs.nm-applet.enable = true; # GUI WIFI tool for WMs
  services.blueman.enable = true;
  programs.hyprlock.enable = true;

  services.fusuma = {
    enable = true;
    "3" = {
      left = {
        command = "${pkgs.swaynotificationcenter}/bin/swaync-client --open-panel";
      };
      right = {
        command = "${pkgs.swaynotificationcenter}/bin/swaync-client --close-panel";
      };
    };
  };
}
