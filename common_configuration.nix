# Configs shared between Nixos and nix-darwin

{
  config,
  lib,
  pkgs,
  inputs,
  system,
  ...
}:

{
  options.smona = with lib; {
    username = mkOption {
      type = types.str;
      description = "smona's username on this system.";
    };
  };

  config = {
    nix = {
      # This will add each flake input as a registry
      # To make nix3 commands consistent with your flake
      registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

      # This will additionally add your inputs to the system's legacy channels
      # Making legacy nix commands consistent as well, awesome!
      nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

      # auto-optimise-store
      optimise.automatic = true;

      settings = {
        # Users allowed to use additional binary caches and unsigned NARs.
        trusted-users = [
          "root"
          config.smona.username
        ];

        # Enable using flakes
        experimental-features = "nix-command flakes";
      };
    };

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    # Automatically backup existing files
    home-manager.backupFileExtension = "bk";
    # Pass additional arguments to home manager modules
    home-manager.extraSpecialArgs = {
      inherit inputs system;
    };
  };
}
