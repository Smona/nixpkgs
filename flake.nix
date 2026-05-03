{
  description = "Smona's nix configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hardware.url = "github:nixos/nixos-hardware";

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    import-tree.url = "github:vic/import-tree";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixGL.url = "github:guibou/nixGL";
    nixGL.inputs.nixpkgs.follows = "nixpkgs";

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    musnix.url = "github:musnix/musnix";

    musnix.inputs.nixpkgs.follows = "nixpkgs";

    wayland-pipewire-idle-inhibit = {
      url = "github:rafaelrc7/wayland-pipewire-idle-inhibit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin.url = "github:catppuccin/nix";
    catppuccin.inputs.nixpkgs.follows = "nixpkgs";
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    roon-mpris = {
      url = "github:Smona/roon-mpris";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dCachix.url = "github:jonascarpay/declarative-cachix";

    # Darwin stuff:
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
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

  outputs =
    inputs@{
      flake-parts,
      import-tree,
      nixpkgs,
      home-manager,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # Support specifying homeConfigurations & homeModules in flake modules.
        home-manager.flakeModules.home-manager
        # Support specifying darwinConfigurations in flake modules.
        # (nix-darwin only declares darwinConfigurations; we add darwinModules below.)
        # TODO: update & remove once this is merged: https://github.com/nix-darwin/nix-darwin/pull/1690
        inputs.nix-darwin.flakeModules.default
        (
          { lib, ... }:
          {
            options.flake.darwinModules = lib.mkOption {
              type = lib.types.lazyAttrsOf lib.types.deferredModule;
              default = { };
              apply = lib.mapAttrs (
                k: v: {
                  _class = "darwin";
                  _file = "flake.darwinModules.${k}";
                  imports = [ v ];
                }
              );
              description = ''
                nix-darwin modules.

                You may use this for reusable pieces of configuration, service modules, etc.
              '';
            };
          }
        )
        (import-tree ./modules)
      ];

      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { system, ... }:
        {
          legacyPackages = import nixpkgs {
            inherit system;
            # NOTE: Using `nixpkgs.config` in your NixOS config won't work
            # Instead, you should set nixpkgs configs here
            # (https://nixos.org/manual/nixpkgs/stable/#idm140737322551056)
            config.allowUnfree = true;
          };
        };

    };
}
