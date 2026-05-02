{ inputs, self, ... }:
{
  flake.nixosConfigurations."xps-nixos" = inputs.nixpkgs.lib.nixosSystem {
    pkgs = self.legacyPackages.x86_64-linux;
    specialArgs = {
      inherit inputs;
    };
    modules = [ ../../nixos/xps-nixos/configuration.nix ];
  };
}
