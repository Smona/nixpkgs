# Configs shared between graphical linux installs and darwin

{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./firefox.nix
    ./terminal.nix
  ];

  options = with lib; {
    graphical = mkEnableOption "installing and configuring graphical applications";

    # High-level toggles for different tasks i want to do on each computer.
    # They're all defined here for ease of discoverability
    roles = {
      art = mkEnableOption "set up computer for visual art creation";
      gaming = mkEnableOption "set up computer for gaming";
      work = mkEnableOption "set up computer for work";
      music = mkEnableOption "set up computer for music production";
    };
  };

  config = lib.mkIf config.graphical {
    programs.kitty.enable = true;

    # programs.chromium = {
    #   enable = true;
    # };
    # programs.firefox.enable = true;
  };
}
