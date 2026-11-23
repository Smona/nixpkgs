# Setup for catppuccin-nix integration

{ inputs, ... }: {
  den.aspects.catppuccin =
    let
      flavor = "mocha";
    in
    {
      homeManager = { ... }: {
        imports = [ inputs.catppuccin.homeModules.catppuccin ];

        catppuccin.enable = true;
        catppuccin.flavor = flavor;
      };

      nixos = { ... }: {
        imports = [ inputs.catppuccin.nixosModules.catppuccin ];

        catppuccin.enable = true;
        catppuccin.flavor = flavor;
      };
    };
}
