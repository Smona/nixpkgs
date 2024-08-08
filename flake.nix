{
  description = "Smona's nix configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # GPG 2.4.1 is currently broken with EPA:
    # https://emacs.stackexchange.com/a/78141/38190
    nixpkgs-downgrade-gpg.url =
      "github:nixos/nixpkgs?rev=5a8650469a9f8a1958ff9373bd27fb8e54c4365d";
    hardware.url = "github:nixos/nixos-hardware";
    #
    # Last known commit (to me) which is compatible with Ubuntu 22 and gnome 44
    nixpkgs-ubuntu.url =
      "github:nixos/nixpkgs?rev=5ba549eafcf3e33405e5f66decd1a72356632b96";
    hm-ubuntu.url =
      "github:Smona/home-manager?rev=66702ccb53a7cc7f84c94e8e571658e1f6b7da69";
    # hm-ubuntu.inputs.nixpkgs.follows = "nixpkgs-ubuntu";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixGL.url = "github:guibou/nixGL";
    nixGL.inputs.nixpkgs.follows = "nixpkgs-ubuntu";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";

    musnix.url = "github:musnix/musnix";
    musnix.inputs.nixpkgs.follows = "nixpkgs";

    # Darwin stuff:
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    nix-homebrew.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.inputs.nix-darwin.follows = "nix-darwin";
    # Optional: Declarative tap management
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-emacs-plus = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = { nixpkgs, home-manager, nixGL, nixos-wsl, nix-darwin, nix-homebrew
    , ... }@inputs: rec {
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
        "xps-nixos" = nixpkgs.lib.nixosSystem {
          pkgs = legacyPackages.x86_64-linux;
          specialArgs = {
            inherit inputs;
            system = "x86_64-linux";
          };
          # > Our main nixos configuration file <
          modules = [ ./nixos/xps-nixos/configuration.nix ];
        };
        "luma-nixos" = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };

          modules = [
            nixos-wsl.nixosModules.wsl
            home-manager.nixosModule
            ./nixos/luma-nixos/configuration.nix
          ];
        };
        "build-farm" = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs;
            system = "x86_64-linux";
          };

          modules = [ ./nixos/build-farm/configuration.nix ];
        };
      };

      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#Mels-MacBook-Air
      darwinConfigurations."Mels-MacBook-Air" = nix-darwin.lib.darwinSystem {
        specialArgs = {
          inherit inputs;
          system = "aarch64-darwin";
        };
        modules = [
          nix-homebrew.darwinModules.nix-homebrew
          home-manager.darwinModules.home-manager
          ./darwin/configuration.nix
        ];
      };

      homeConfigurations = {
        "cobalt@remotestation376" =
          inputs.hm-ubuntu.lib.homeManagerConfiguration {
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
              ({ ... }: {
                home.username = "cobalt";
                gnome.enable = true;
                logitech.enabled = true;
                roles = { work = true; };
                nixGL.prefix =
                  "${nixGL.packages.x86_64-linux.nixGLIntel}/bin/nixGLIntel ";
              })
            ];
          };
        "smona@DESKTOP-9F9VN3S" = home-manager.lib.homeManagerConfiguration {
          pkgs = legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home.nix ({ ... }: { home.username = "smona"; }) ];
        };
      };
    };
}
