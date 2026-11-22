# Modules for music & audio production computers
{ inputs, ... }:
{
  den.aspects.music-production = {
    user.extraGroups = [ "audio" ];

    hmLinux =
      {
        config,
        lib,
        pkgs,
        ...
      }:

      lib.mkIf config.roles.music {
        home.packages =
          with pkgs;
          [ real_time_config_quick_scan ]
          ++ (lib.lists.optionals config.graphical [
            bitwig-studio3
            qpwgraph
          ]);
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

        # https://github.com/musnix/musnix#base-options
        musnix.enable = true;

        # Not currently working on my nixpkgs version
        # error: attribute 'linuxPackages_5_4_rt' missing
        # musnix.kernel.realtime = true;

        services.pipewire.jack.enable = true;
        environment.systemPackages = with pkgs; [ qjackctl ];
      };
  };

    };
}
