# Enable CUPS to print documents.
{ ... }:
{
  flake.nixosModules.printing =
    { pkgs, ... }:
    {
      services.printing = {
        enable = true;
        # Necessary drivers for Canon MX860
        drivers = with pkgs; [
          cups-bjnp
          gutenprint
        ];
      };
    };
}
