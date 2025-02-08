# NixOS configuration shared between all NixOS machines

{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    inputs.home-manager.nixosModule
    ../common_configuration.nix
    inputs.catppuccin.nixosModules.catppuccin
    ./samba.nix
    ./wlroots.nix
    ./boot.nix
    ./greetd.nix
    ./realtime_audio.nix
  ];

  options.smona = with lib; {
    primaryMonitor = mkOption {
      description = "Which monitor ID represents the 'primary' monitor.";
      type = types.str;
    };
    wallpaper = mkOption {
      description = "Image to use as the system-wide wallpaper.";
      type = types.path;
    };
  };

  config = {
    catppuccin.enable = true;
    catppuccin.flavor = "mocha";

    # Set your time zone.
    time.timeZone = "America/Chicago";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.utf8";

    # We don't need to instll X11. We are so wayland!!
    # services.xserver.enable = true;
    # Configure keymap in X11
    services.xserver.xkb = {
      layout = "us";
      variant = "dvorak";
    };

    # Enable the GNOME Desktop Environment.
    # services.xserver.displayManager.gdm.enable = true;
    # TODO: ensure that gnome is not enabled at the same time as wlroots.
    # This should probably just be an enum configuration option to guarantee that.
    # services.xserver.desktopManager.gnome.enable = true;

    # Configure console keymap
    console.keyMap = "dvorak";

    # Set up zsh as the default user shell
    users.defaultUserShell = pkgs.zsh;

    # Enable networking with networkmanager
    networking.networkmanager.enable = true;

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
    environment.systemPackages = with pkgs; [
      nautilus
      keymapp
    ];
    # Enable file previews for nautilus with spacebar.
    services.gnome.sushi.enable = true;

    # Enable flashing ZSA keyboards.
    hardware.keyboard.zsa.enable = true;

    # Firmware
    services.fwupd.enable = true; # Firmware update manager
    # Make sure we have the best firmware available.
    # This is especially important for dual-booting, since firmware installed in
    # Windows can break bluetooth or other features
    hardware.enableAllFirmware = true;

    # Enable CUPS to print documents.
    services.printing = {
      enable = true;
      # Necessary drivers for Canon MX860
      drivers = with pkgs; [
        cups-bjnp
        gutenprint
      ];
    };

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.${config.smona.username} = {
      isNormalUser = true;
      description = "Mel Bourgeois";
      extraGroups = [
        "networkmanager"
        "wheel"
        "video"
        "input"
        "docker"
      ];
      packages = with pkgs; [ firefox ];
    };

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    };

    programs._1password-gui = {
      enable = true;
      polkitPolicyOwners = [ config.smona.username ];
    };

    # Ports required by spotify connect:
    # https://nixos.wiki/wiki/Spotify
    networking.firewall.allowedTCPPorts = [ 57621 ];
    networking.firewall.allowedUDPPorts = [ 5353 ];
    # Always use Cloudflare nameservers
    networking.nameservers = [
      "1.1.1.1"
      "1.0.0.1"
    ];

    # FIXME: now that MLS is retired we're using google location services, and
    # just borrowing Arch's geolocation API key, 💙
    # TODO: get filesystem secret deployment set up with our own API key,
    # or find a suitable public alternative
    # https://github.com/NixOS/nixpkgs/issues/321121
    services.geoclue2.geoProviderUrl = ''
      https://www.googleapis.com/geolocation/v1/geolocate?key=AIzaSyDwr302FpOSkGRpLlUpPThNTDPbXcIn_FM
    '';

    # Required for fx_cast, and possible geoclue
    services.avahi = {
      enable = true;
      nssmdns4 = true;
    };
  };
}
