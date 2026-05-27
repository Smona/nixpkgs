# Modules for setting up spotify.
{ ... }:
{
  flake.nixosModules.spotify =
    { ... }:
    {
      # Ports required by spotify connect:
      # https://nixos.wiki/wiki/Spotify
      networking.firewall.allowedTCPPorts = [ 57621 ];
      networking.firewall.allowedUDPPorts = [ 5353 ];
    };
}
