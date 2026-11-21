{ ... }:
{
  den.aspects.dualboot.nixos =
    { ... }:
    {
      # Make system time compatible b/w nixos & windows
      time.hardwareClockInLocalTime = true;
    };
}
