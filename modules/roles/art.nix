{ ... }:
{
  den.aspects.art = {
    hmLinux =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      lib.mkIf config.roles.art {
        home.packages = with pkgs; [ (config.lib.nixGL.wrap krita) ];
      };
  };
}
