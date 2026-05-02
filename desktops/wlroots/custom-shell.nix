{
  config,
  lib,
  pkgs,
  ...
}:

let
  my_rofi = pkgs.rofi.override { plugins = with pkgs; [ rofi-calc ]; };
in
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

  programs.rofi = {
    enable = true;
    package = my_rofi;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./rofi-theme.rasi;
  };
  catppuccin.rofi.enable = false;

  services.fusuma = {
    enable = true;
    extraPackages = with pkgs; [
      my_rofi
      coreutils-full
      wtype
    ];
    settings = {
      threshold = {
        swipe = 0.1;
      };
      interval = {
        swipe = 0.7;
      };
      swipe = {
        "3" = {
          left = {
            command = "${pkgs.swaynotificationcenter}/bin/swaync-client --open-panel";
          };
          right = {
            command = "${pkgs.swaynotificationcenter}/bin/swaync-client --close-panel";
          };
        };
        "4" = {
          left = {
            command = "${pkgs.sway}/bin/swaymsg workspace next";
          };
          right = {
            command = "${pkgs.sway}/bin/swaymsg workspace prev";
          };
        };
      };
    };
  };
}
