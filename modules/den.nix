{
  inputs,
  den,
  lib,
  ...
}:

{
  imports = [ inputs.den.flakeModule ];

  # TODO: see if i can get this working
  # An aspect that adds hmLinux, hmDarwin, hmAarch64, and hm64bit classes
  # for configuring packages installed via home manager that only work
  # on some platforms.
  # see: https://den.denful.dev/guides/custom-class-examples/#platform-specific-hm-classes
  # den.aspects.hmPlatforms =
  #   { class, user }:
  #   den.batteries.forward {
  #     each = [
  #       "Linux"
  #       "Darwin"
  #       "Aarch64"
  #       "64bit"
  #     ];
  #     fromClass = platform: "hm${platform}";
  #     fromAspect = _: user.aspect;
  #     intoClass = _: "homeManager";
  #     intoPath = _: [ ];
  #     guard = { pkgs, ... }: platform: lib.mkIf pkgs.stdenv."is${platform}";
  #     # adaptArgs = { config, ... }: { osConfig = config; };
  #   };

  den.schema.user = {
    classes = lib.mkDefault [ "homeManager" ];
  };

  # Allow my own user to configure nixos options across all hosts.
  den.schema.host.includes = [
    (den.batteries.user-aspects {
      users = [
        "mel"
        "smona"
      ];
    })

    # TODO: move into appropriate aspect definitions
    (den.batteries.unfree [ "1password" ])
    (den.batteries.unfree [ "bitwig-studio" ])
    (den.batteries.unfree [ "slack" ])
    (den.batteries.unfree [ "pridecat" ])
    (den.batteries.unfree [ "obsidian" ])
    (den.batteries.unfree [ "keymapp" ])
    (den.batteries.unfree [
      # TODO: where are these coming from? came up on luma specifically
      "broadcom-bt-firmware"
      "b43-firmware"
      "xone-dongle-firmware"
      "facetimehd-calibration"
      "facetimehd-firmware"
    ])
    (den.batteries.unfree [
      "corefonts"
      "vista-fonts"
    ])
  ];

  den.hosts.x86_64-linux.luma = {
    users.mel = { };

    # temporary workaround until i finish migrating non-flake-parts modules
    instantiate =
      args:
      inputs.nixpkgs.lib.nixosSystem (
        args
        // {
          specialArgs = (args.specialArgs or { }) // {
            inherit inputs;
          };
        }
      );
  };
  den.hosts.x86_64-linux.xps-nixos = {
    users.smona = { };

    # temporary workaround until i finish migrating non-flake-parts modules
    instantiate =
      args:
      inputs.nixpkgs.lib.nixosSystem (
        args
        // {
          specialArgs = (args.specialArgs or { }) // {
            inherit inputs;
          };
        }
      );
  };
  den.hosts.x86_64-linux.luma-nixos = {
    users.smona = { };
    wsl.enable = true;

    # temporary workaround until i finish migrating non-flake-parts modules
    instantiate =
      args:
      inputs.nixpkgs.lib.nixosSystem (
        args
        // {
          specialArgs = (args.specialArgs or { }) // {
            inherit inputs;
          };
        }
      );
  };
  den.homes.x86_64-linux."cobalt@remotestation376" = {
    # temporary workaround until i finish migrating non-flake-parts modules
    instantiate =
      args:
      inputs.home-manager.lib.homeManagerConfiguration (
        args
        // {
          extraSpecialArgs = (args.extraSpecialArgs or { }) // {
            inherit inputs;
          };
        }
      );
  };
  den.homes.x86_64-linux."smona@DESKTOP-9F9VN3S" = {
    # temporary workaround until i finish migrating non-flake-parts modules
    instantiate =
      args:
      inputs.home-manager.lib.homeManagerConfiguration (
        args
        // {
          extraSpecialArgs = (args.extraSpecialArgs or { }) // {
            inherit inputs;
          };
        }
      );
  };
  den.hosts.aarch64-darwin.Mels-Macbook-Air = {
    users.mel = { };

    # temporary workaround until i finish migrating non-flake-parts modules
    instantiate =
      args:
      inputs.nix-darwin.lib.darwinSystem (
        args
        // {
          specialArgs = (args.specialArgs or { }) // {
            inherit inputs;
          };
        }
      );
  };

  den.default = {
    includes = [
      # TODO: finish removing duplicated definitions already defined by this battery once
      # all hosts are migrated.
      den.batteries.define-user
      # TODO: finish removing duplicated definitions already defined by this battery once
      # all hosts are migrated.
      den.batteries.hostname
    ];

    # Setting consistent global stateVersion defaults, using the oldest
    # stateVersion in the repo.
    #
    # Before changing these values read their documentation
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html),
    # and perform any necessary migrations on all hosts.
    nixos.system.stateVersion = lib.mkDefault "22.05";
    homeManager.home.stateVersion = lib.mkDefault "22.05";
    darwin.system.stateVersion = lib.mkDefault 4;
  };
}
