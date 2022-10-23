{ config, lib, pkgs, inputs, ... }:

{
  imports = [ ../../applications/gui.nix ./waybar ./sway.nix ./hyprland.nix ];

  smona.waybar.enable = true;

  home.packages = with pkgs; [
    inotify-tools
    wofi
    swaybg
    # Needed for flameshot
    grim
    slurp
  ];

  services.flameshot = {
    enable = true;
    # settings = {};
  };

  # Keeps track of media players so playerctl always acts on the most
  # recently active one.
  services.playerctld.enable = true;

  wayland.windowManager.sway.enable = true;
  wayland.windowManager.hyprland.enable = true;
}
