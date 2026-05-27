# Modules for setting up spotify.
{ inputs, ... }:
{
  flake.nixosModules.spotify =
    { ... }:
    {
      # Ports required by spotify connect:
      # https://nixos.wiki/wiki/Spotify
      networking.firewall.allowedTCPPorts = [ 57621 ];
      networking.firewall.allowedUDPPorts = [ 5353 ];
    };

  flake.homeModules.spotify =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      imports = [ inputs.spicetify-nix.homeManagerModules.default ];

      programs.spicetify = lib.mkIf config.graphical (
        let
          spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
        in
        {
          enable = true;
          # TODO: check out spicetify extensions
          enabledExtensions = with spicePkgs.extensions; [
            #   hidePodcasts
            shuffle # shuffle+ (special characters are sanitized out of extension names)
            fullAppDisplay
          ];
          theme = spicePkgs.themes.catppuccin;
          colorScheme = config.catppuccin.flavor;
        }
      );
    };
}
