# Enable CUPS to print documents on printers I use.
{ ... }:
{
  den.aspects.printing.nixos =
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
