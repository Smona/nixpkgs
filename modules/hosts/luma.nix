# Baremetal installation
{ inputs, self, ... }:
{
  flake.nixosConfigurations."luma" = inputs.nixpkgs.lib.nixosSystem {
    pkgs = self.legacyPackages.x86_64-linux;
    specialArgs = {
      inherit inputs;
    };
    modules = [ ../../nixos/luma/configuration.nix ];
  };
}
