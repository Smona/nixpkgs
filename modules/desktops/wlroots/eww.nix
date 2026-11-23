{ self, ... }:
{
  den.aspects.eww.hmLinux =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      cfg = config.smona.eww;
    in
    {
      options.smona.eww = {
        enable = lib.mkEnableOption "eww widgets";
      };

      config = lib.mkIf cfg.enable {
        home.packages = [ self.packages.${pkgs.stdenv.hostPlatform.system}.hypr_info ];
        programs.eww = {
          enable = true;
          configDir = ./eww;
        };
      };
    };
}
