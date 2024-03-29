# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs, system, config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan.
    ./wlroots.nix
    ../realtime_audio.nix
    ../samba.nix
    inputs.home-manager.nixosModule
    inputs.hardware.nixosModules.dell-xps-13-7390
  ];

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

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "xps-nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  # TODO: only install when wlroots is enabled (once nixos config is modularized)
  programs.nm-applet.enable = true; # GUI WIFI tool for WMs

  # Always use Cloudflare nameservers
  networking.nameservers = [ "1.1.1.1" "1.0.0.1" ];

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "dvorak";
  };

  # Configure console keymap
  console.keyMap = "dvorak";

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    # Necessary drivers for Canon MX860
    drivers = with pkgs; [ cups-bjnp gutenprint ];
  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.smona = {
    isNormalUser = true;
    description = "Mel Bourgeois";
    extraGroups = [ "networkmanager" "wheel" "video" "input" "docker" ];
    packages = with pkgs;
      [
        firefox
        #  thunderbird
      ];
  };
  home-manager.users.smona = { pkgs, ... }: {
    imports = [ ../../home.nix ];

    home.username = "smona";
    smona.wlroots = {
      enable = true;
      builtInDisplay = "eDP-1";
    };
    roles = {
      art = true;
      gaming = true;
      music = true;
    };
    logitech.enabled = true;
    # Should apply to any NixOS machine
    _1passwordBinary = "${pkgs._1password-gui}/bin/1password";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.extraSpecialArgs = { inherit inputs system; };

  # Packages installed in the system profile
  environment.systemPackages = with pkgs; [ git vim ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Set up zsh as the default user shell
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall =
      true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall =
      true; # Open ports in the firewall for Source Dedicated Server
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "smona" ];
  virtualisation.docker.enable = true;

  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "smona" ];
  };

  # Required for fx_cast
  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  # Laptop stuff
  services.power-profiles-daemon.enable = false;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
    };
  };

  # Location service provider, required for gammastep
  services.geoclue2.enable = true;

  # Enable IIO for autorotation and light detection
  hardware.sensor.iio.enable = true;

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=4h
    # https://github.com/NixOS/nixos-hardware/tree/master/dell/xps/13-7390
    SuspendState=freeze
  '';
  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    lidSwitchExternalPower = "suspend-then-hibernate";
    extraConfig = ''
      HandlePowerKey=suspend-then-hibernate
    '';
  };

  # Firmware update manager
  services.fwupd.enable = true;
  services.flatpak.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
