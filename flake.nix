{
  description = "Smona's nix configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # hardware.url = "github:nixos/nixos-hardware";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixGL.url = "github:guibou/nixGL";
    nixGL.inputs.nixpkgs.follows = "nixpkgs";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = { nixpkgs, home-manager, nixGL, ... }@inputs: rec {
    # This instantiates nixpkgs for each system listed
    # Allowing you to configure it (e.g. allowUnfree)
    # Our configurations will use these instances
    legacyPackages = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        import inputs.nixpkgs {
          inherit system;

          # NOTE: Using `nixpkgs.config` in your NixOS config won't work
          # Instead, you should set nixpkgs configs here
          # (https://nixos.org/manual/nixpkgs/stable/#idm140737322551056)
          config.allowUnfree = true;
        });

    nixosConfigurations = {
      xps-nixos = nixpkgs.lib.nixosSystem {
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; }; # Pass flake inputs to our config
        # > Our main nixos configuration file <
        modules = [ ./nixos/xps-nixos/configuration.nix ];
      };
    };

    homeConfigurations = {
      "smona@xps-nixos" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs; # Pass flake inputs to our config
          hostName = "xps_nixos";
          username = "smona";
          nixGLPrefix = "";
          desktops = {
            gnome = {
              enable = true;
              theme = "flat-remix";
            };
          };
          roles = {
            gaming = true;
            work = false;
          };
        };
        # > Our main home-manager configuration file <
        modules = [ ./home.nix ];
      };
      "cobalt@remotestation376" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs; # Pass flake inputs to our config
          hostName = "remotestation376";
          username = "cobalt";
          nixGLPrefix =
            "${nixGL.packages.x86_64-linux.nixGLIntel}/bin/nixGLIntel ";
          desktops = {
            gnome = {
              enable = true;
              theme = "flat-remix";
            };
          };
          roles = {
            gaming = false;
            work = true;
          };
        };
        modules = [ ./home.nix ];
      };
      "smona@DESKTOP-9F9VN3S" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs; # Pass flake inputs to our config
          hostName = "DESKTOP-9F9VN3S";
          username = "smona";
          desktops = {
            gnome = {
              enable = false;
              theme = "flat-remix";
            };
          };
          roles = {
            gaming = false;
            work = false;
          };
        };
        modules = [ ./home.nix ];
      };

    };
  };
}
