{ inputs, den, ... }:
{
  den.homes.x86_64-linux."cobalt@remotestation376" = { };
  den.hosts.x86_64-linux.remotestation376 = { };

  den.aspects."cobalt@remotestation376" = {
    # Inherited username on this machine, bring my home configs in
    includes = [ den.aspects.smona ];

    homeManager = { ... }: {
      smona.desktop.compositor = "gnome";
      roles = {
        work = true;
      };
      targets.genericLinux.nixGL.packages = inputs.nixGL.packages;

      # Ubuntu 24 currently seems pretty compatible with nixpkgs unstable
      # pkgsCompat = import inputs.nixpkgs-ubuntu {
      #   system = "x86_64-linux";
      #   config.allowUnfree = true;
      # };
    };
  };
}
