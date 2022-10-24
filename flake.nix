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

    musnix.url = "github:musnix/musnix";
    musnix.inputs.nixpkgs.follows = "nixpkgs";

    hyprland.url = "github:hyprwm/Hyprland";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = { nixpkgs, home-manager, nixGL, emacs-overlay, ... }@inputs: rec {
    legacyPackages = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        import inputs.nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.emacs ];

          # NOTE: Using `nixpkgs.config` in your NixOS config won't work
          # Instead, you should set nixpkgs configs here
          # (https://nixos.org/manual/nixpkgs/stable/#idm140737322551056)
          config.allowUnfree = true;
        });

    nixosConfigurations = {
      "xps-nixos" = nixpkgs.lib.nixosSystem {
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        # > Our main nixos configuration file <
        modules = [ ./nixos/xps-nixos/configuration.nix ];
      };
    };

    homeConfigurations = {
      "cobalt@remotestation376" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./home.nix
          ({ ... }: {
            home.username = "cobalt";
            gnome.enable = true;
            roles = { work = true; };
            nixGLPrefix =
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
