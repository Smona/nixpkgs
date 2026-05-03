# NixOS module for hosts with the gaming role.
{ ... }:
{
  flake.nixosModules.gaming =
    { ... }:
    {
      programs.steam = {
        enable = true;
        remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
        dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      };
    };
}
