{ inputs, ... }:
{
  flake.homeConfigurations."cobalt@remotestation376" = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      config.allowUnfree = true;
    };

    extraSpecialArgs = {
      inherit inputs;
    };

    modules = [
      ../../home.nix
      (
        { ... }:
        {
          home.username = "cobalt";
          gnome.enable = true;
          logitech.enabled = true;
          roles = {
            work = true;
          };
          nixGL.packages = inputs.nixGL.packages;

          # Ubuntu 24 currently seems pretty compatible with nixpkgs unstable
          # pkgsCompat = import inputs.nixpkgs-ubuntu {
          #   system = "x86_64-linux";
          #   config.allowUnfree = true;
          # };
        }
      )
    ];
  };
}
