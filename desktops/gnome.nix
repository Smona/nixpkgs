{ config, lib, pkgs, ... }:

{
  imports = [ ./gui.nix ];

  home.packages = with pkgs; [
    # Gnome goodness
    gnomeExtensions.burn-my-windows
    gnomeExtensions.dash-to-dock
    gnomeExtensions.espresso # FIXME upgrade to jammy jellyfish
    gnomeExtensions.hide-keyboard-layout
    gnomeExtensions.middle-click-to-close-in-overview
    gnomeExtensions.noannoyance-2 # TODO: settings dialog broken
    gnomeExtensions.system-monitor # FIXME
    gnomeExtensions.transparent-window
    gnomeExtensions.unite
  ];
}
