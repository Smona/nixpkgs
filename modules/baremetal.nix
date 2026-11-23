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

      # Use GDM as a display manager
      services.displayManager.gdm.enable = true;

      # Firmware
      services.fwupd.enable = true; # Firmware update manager
      # Make sure we have the best firmware available.
      # This is especially important for dual-booting, since firmware installed in
      # Windows can break bluetooth or other features
      hardware.enableAllFirmware = true;
    };
  };
}
