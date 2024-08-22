{ inputs, system, config, pkgs, lib, ... }:

{
  imports = [ inputs.home-manager.nixosModule ./samba.nix ./wlroots.nix ];

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;
    # auto-optimise-store
    optimise.automatic = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  home-manager.useGlobalPkgs = true;
  home-manager.backupFileExtension = "bk";
  home-manager.extraSpecialArgs = { inherit inputs system; };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "dvorak";
  };

  # Configure console keymap
  console.keyMap = "dvorak";

  # Set up zsh as the default user shell
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Packages installed in the system profile
  environment.systemPackages = with pkgs; [ git vim ];

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    # Necessary drivers for Canon MX860
    drivers = with pkgs; [ cups-bjnp gutenprint ];
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall =
      true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall =
      true; # Open ports in the firewall for Source Dedicated Server
  };

}
