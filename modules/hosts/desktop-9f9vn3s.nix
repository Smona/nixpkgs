{ inputs, self, ... }:
{
  flake.homeConfigurations."smona@DESKTOP-9F9VN3S" =
    inputs.home-manager.lib.homeManagerConfiguration
      {
        pkgs = self.legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs;
        };
        modules = [
          ../../home.nix
          {
            home.username = "smona";
          }
        ];
      };
}
