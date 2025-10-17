# Configuration specific to AMD CPUs
{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Enable CPU frequency governors
  boot.kernelParams = [ "amd_pstate=guided" ];
}
