# NixOS & Home Manager modules for hosts with the gaming role.
{ ... }:
{
  flake.nixosModules.gaming =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      programs.steam = {
        enable = true;
        # Use millennium on noctalia to enable theming.
        # Note that there are a couple manual steps needed: installing material theme and selecting the
        # "Matugen" color scheme. See the docs:
        # https://docs.noctalia.dev/v4/theming/program-specific/steam/
        # TODO: see if this can be done declaratively
        package = lib.mkIf (config.smona.desktop.shell == "noctalia") pkgs.millennium-steam;
        remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
        dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      };
    };

  flake.homeModules.gaming =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        prismlauncher
        jdk17
      ];
    };
}
