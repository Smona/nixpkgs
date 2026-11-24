# NixOS & Home Manager modules for hosts that I play games on.
{ den, ... }:
{
  den.aspects.gaming = {
    includes = [
      (den.batteries.unfree [
        "steam"
        "steam-unwrapped"
      ])
    ];

    nixos =
      { ... }:
      {
        programs.steam = {
          enable = true;
          # Use millennium on noctalia to enable theming.
          # Note that there are a couple manual steps needed: installing material theme and selecting the
          # "Matugen" color scheme. See the docs:
          # https://docs.noctalia.dev/v4/theming/program-specific/steam/
          # TODO: see if this can be done declaratively
          remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
          dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
        };
      };

    darwin.homebrew.casks = [ "steam" ];

    homeManager =
      {
        pkgs,
        lib,
        config,
        ...
      }:
      {
        config = lib.mkIf config.roles.gaming {
          home.packages = with pkgs; [
            prismlauncher
            jdk17
          ];
        };
      };
  };
}
