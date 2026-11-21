# NixOS modules for systems by CPU vendor.
{ ... }:
{
  den.aspects.amdcpu.nixos =
    { ... }:
    {
      # Enable CPU frequency governors
      boot.kernelParams = [ "amd_pstate=guided" ];
    };
}
