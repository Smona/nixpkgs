# NixOS modules for systems by CPU vendor.
{ ... }:
{
  flake.nixosModules.amd-cpu =
    { ... }:
    {
      # Enable CPU frequency governors
      boot.kernelParams = [ "amd_pstate=guided" ];
    };
}
