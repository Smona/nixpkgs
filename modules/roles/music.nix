# Modules for music & audio production computers
{ den, inputs, ... }:
{
  den.aspects.music-production = {
    includes = [ (den.batteries.unfree [ "bitwig-studio" ]) ];

    hmLinux =
      {
        config,
        lib,
        pkgs,
        ...
      }:

      lib.mkIf config.roles.music {
        home.packages = with pkgs; [
          real_time_config_quick_scan
          bitwig-studio3
          qpwgraph
        ];
      };

    nixos =
      {
        config,
        pkgs,
        user,
        ...
      }:
      {
        imports = [ inputs.musnix.nixosModules.musnix ];

        users.users.${user.userName}.extraGroups = [ "audio" ];

        # https://github.com/musnix/musnix#base-options
        musnix.enable = true;

        # Not currently working on my nixpkgs version
        # error: attribute 'linuxPackages_5_4_rt' missing
        # musnix.kernel.realtime = true;

        services.pipewire.jack.enable = true;
        environment.systemPackages = with pkgs; [ qjackctl ];
      };
  };
}
