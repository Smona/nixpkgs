# Better behavior under high RAM pressure: zram swap + earlyoom.
{ ... }:
{
  flake.nixosModules.oom-handling =
    { ... }:
    {
      # Use compressed RAM as swap
      zramSwap.enable = true;
      # earlyoom handles OOM more gracefully than systemd-oomd.
      # might just need to find the right systemd-oomd configuration.
      services.earlyoom = {
        enable = true;
        freeMemThreshold = 7;
        freeSwapThreshold = 50;
        enableNotifications = true;
      };
      # https://github.com/NixOS/nixpkgs/issues/374959
      systemd.services.earlyoom.serviceConfig.DynamicUser = false;
      systemd.oomd.enable = false;
    };
}
