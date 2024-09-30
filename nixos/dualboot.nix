{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Make system time compatible b/w nixos & windows
  time.hardwareClockInLocalTime = true;
}
