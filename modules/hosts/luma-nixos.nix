# WSL installation
{ inputs, ... }:
{
  flake.nixosConfigurations."luma-nixos" = inputs.nixpkgs.lib.nixosSystem {
    specialArgs = {
      inherit inputs;
    };

    modules = [
      inputs.nixos-wsl.nixosModules.wsl
      inputs.home-manager.nixosModules.home-manager
      ../../nixos/luma-nixos/configuration.nix
    ];
  };
}
