# Configs for computers where nix manages all the way down to the bootloader
# (aka NixOS computers that don't run in WSL)
{ den, ... }:

{
  den.aspects.baremetal = {
    includes = [
      den.aspects.boot
      den.aspects.oom-handling
      den.aspects.printing # Don't need to print from WSL
    ];

    nixos = { ... }: {
      # Enable users to install flatpaks.
      # Doesn't work on WSL where xdg.portal.enable = false;
      services.flatpak.enable = true;
    };
  };
}
