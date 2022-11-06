{ config, lib, pkgs, ... }:

let cfg = config.smona.wlroots;
in {
  imports =
    [ ../../applications/gui.nix ./waybar ./eww.nix ./sway.nix ./hyprland.nix ];

  options.smona.wlroots = {
    enable = lib.mkEnableOption "wlroots window managers";
  };

  config = lib.mkIf cfg.enable {
    graphical = true;

    smona.waybar.enable = true;
    smona.eww.enable = true;

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
  };
}
