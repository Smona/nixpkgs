# Configs shared between Nixos and nix-darwin

{
  config,
  lib,
  pkgs,
  inputs,
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

      # Periodically optimise the store
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
      inherit inputs;
    };

    # Configure the system-wide /etc/zshrc, and install zsh for root on linux.
    # Note that this is required on darwin, since it loads the nix-darwin environment in the default shell on macos.
    programs.zsh.enable = true;

    # Set the system timezone automatically
    time.timeZone = lib.mkDefault "America/Denver";
    services.automatic-timezoned.enable = true;

    environment.systemPackages = with pkgs; [
      git
      vim
    ];
  };
}
