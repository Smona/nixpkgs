{
  description = "Smona's nix configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hardware.url = "github:nixos/nixos-hardware";

    # Last known commit (to me) which is compatible with Ubuntu 22 and gnome 44
    nixpkgs-ubuntu.url = "github:nixos/nixpkgs?rev=5ba549eafcf3e33405e5f66decd1a72356632b96";
    hm-ubuntu.url = "github:Smona/home-manager?rev=66702ccb53a7cc7f84c94e8e571658e1f6b7da69";
    # hm-ubuntu.inputs.nixpkgs.follows = "nixpkgs-ubuntu";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixGL.url = "github:guibou/nixGL";
    nixGL.inputs.nixpkgs.follows = "nixpkgs-ubuntu";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";

    musnix.url = "github:musnix/musnix";
    musnix.inputs.nixpkgs.follows = "nixpkgs";

    wayland-pipewire-idle-inhibit = {
      url = "github:rafaelrc7/wayland-pipewire-idle-inhibit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin.url = "github:catppuccin/nix";
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Inputs I occasionally use temporarily
    # hyprland.url =
    #   "github:hyprwm/Hyprland?rev=1c460e98f870676b15871fe4e5bfeb1a32a3d6d8";
    # my-nixpkgs.url = "git+file:/home/smona/dev/nixpkgs-upstream";

    dCachix.url = "github:jonascarpay/declarative-cachix";
    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nixGL,
      nixos-wsl,
      ...
    }@inputs:
    rec {
      legacyPackages =
        nixpkgs.lib.genAttrs
          [
            "x86_64-linux"
            "x86_64-darwin"
          ]
          (
            system:
            import inputs.nixpkgs {
              inherit system;
              # overlays = [
              #   (final: prev: {
              #     hyprland = inputs.hyprland.packages.${system}.hyprland;
              #   })
              # ];

              # NOTE: Using `nixpkgs.config` in your NixOS config won't work
              # Instead, you should set nixpkgs configs here
              # (https://nixos.org/manual/nixpkgs/stable/#idm140737322551056)
              config.allowUnfree = true;
            }
          );

      nixosConfigurations = {
        "xps-nixos" = nixpkgs.lib.nixosSystem {
          pkgs = legacyPackages.x86_64-linux;
          specialArgs = {
            inherit inputs;
            system = "x86_64-linux";
          };
          modules = [ ./nixos/xps-nixos/configuration.nix ];
        };
        # WSL installation
        "luma-nixos" = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs;
          };

          modules = [
            nixos-wsl.nixosModules.wsl
            home-manager.nixosModule
            ./nixos/luma-nixos/configuration.nix
          ];
        };
        # Baremetal installation
        "luma" = nixpkgs.lib.nixosSystem {
          pkgs = legacyPackages.x86_64-linux;
          specialArgs = {
            inherit inputs;
            system = "x86_64-linux";
          };
          modules = [ ./nixos/luma/configuration.nix ];
        };
        "build-farm" = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs;
            system = "x86_64-linux";
          };

          modules = [ ./nixos/build-farm/configuration.nix ];
        };
      };

      homeConfigurations = {
        "cobalt@remotestation376" = inputs.hm-ubuntu.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          extraSpecialArgs = {
            inherit inputs;
            system = "x86_64-linux";
          };
          modules = [
            ./home.nix
            (
              { ... }:
              {
                home.username = "cobalt";
                gnome.enable = true;
                logitech.enabled = true;
                roles = {
                  work = true;
                };
                nixGL.prefix = "${nixGL.packages.x86_64-linux.nixGLIntel}/bin/nixGLIntel ";
                pkgsCompat = import inputs.nixpkgs-ubuntu {
                  system = "x86_64-linux";
                  config.allowUnfree = true;
                };
              }
            )
          ];
        };
        "smona@DESKTOP-9F9VN3S" = home-manager.lib.homeManagerConfiguration {
          pkgs = legacyPackages.x86_64-linux;
          extraSpecialArgs = {
            inherit inputs;
          };
          modules = [
            ./home.nix
            (
              { ... }:
              {
                home.username = "smona";
              }
            )
          ];
        };
      };
    };
}
