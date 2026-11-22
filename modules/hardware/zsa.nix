{ den, ... }:

{
  den.aspects.zsa = {
    includes = [
      (den.batteries.unfree [ "keymapp" ])
    ];

    nixos = { pkgs, ... }: {
      # Enable flashing ZSA keyboards.
      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = [ pkgs.keymapp ];
    };

    darwin = { ... }: {
      # ZSA's companion UI, e.g. for flashing
      homebrew.casks = [ "keymapp" ];
    };
  };
}
