{ inputs, ... }:
{
  flake.nixosConfigurations."build-farm" = inputs.nixpkgs.lib.nixosSystem {
    modules = [ ../../nixos/build-farm/configuration.nix ];
  };
}
