{ config, lib, pkgs, ... }:

{
  imports = [ ./gui.nix ];

  home.packages = with pkgs.gnomeExtensions; [
    # Gnome goodness
    always-indicator
    audio-switcher-40
    autohide-battery
    burn-my-windows
    clipboard-indicator
    dash-to-dock
    espresso # Requires gnome 40 (Ubuntu >= jammy jellyfish)
    gesture-improvements
    hide-keyboard-layout
    middle-click-to-close-in-overview
    noannoyance-2 # TODO: settings dialog broken
    remove-alttab-delay-v2
    system-monitor # Requires Ubuntu > 20.04
    transparent-window # TODO: this doesn't seem to work
    unite
    user-themes
    windownavigator
  ];
}
