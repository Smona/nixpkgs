{ self, ... }:
{
  den.aspects.eww.hmLinux =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      home.packages = [ self.packages.${pkgs.stdenv.hostPlatform.system}.hypr_info ];
      programs.eww = {
        enable = true;
        configDir = ./eww;
      };
    };
}
