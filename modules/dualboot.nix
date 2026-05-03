{ ... }:
{
  flake.nixosModules.dualboot =
    { ... }:
    {
      # Make system time compatible b/w nixos & windows
      time.hardwareClockInLocalTime = true;
    };
}
