# NixOS configuration shared across all personal computers
{ inputs, self, ... }:
{
  flake.nixosModules.pc =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
        ../common_configuration.nix
        inputs.catppuccin.nixosModules.catppuccin
        self.nixosModules.lilnasx
        ../nixos/wlroots.nix
        ../nixos/boot.nix
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

        # Select internationalisation properties.
        i18n.defaultLocale = "en_US.UTF-8";

        # We don't need to install X11. We are so wayland!!
        # services.xserver.enable = true;
        # Configure keymap in X11
        services.xserver.xkb = {
          layout = "us";
          variant = "dvorak";
        };

        # Enable the GNOME Desktop Environment.
        services.displayManager.gdm.enable = true;
        # TODO: ensure that gnome is not enabled at the same time as wlroots.
        # This should probably just be an enum configuration option to guarantee that.
        # services.xserver.desktopManager.gnome.enable = true;

        # Configure console keymap
        console.keyMap = "dvorak";

        # Set up zsh as the default user shell
        users.defaultUserShell = pkgs.zsh;

        # Enable networking with networkmanager
        networking.networkmanager = {
          enable = true;
          # Can improve WiFi connection reliability
          wifi.powersave = false;
          plugins = with pkgs; [ networkmanager-openvpn ];
        };

        powerManagement.cpuFreqGovernor = lib.mkForce "schedutil";

        # Enable sound with pipewire.
        security.rtkit.enable = true;
        services.pulseaudio.enable = false;
        services.pipewire = {
          enable = true;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
          extraConfig.pipewire = {
            "10-clock-rate" = {
              # enable high sample rate playback
              "context.properties" = {
                "default.clock.allowed-rates" = [
                  44100
                  48000
                  88200
                  96000
                  176400
                  192000
                ];
              };
            };
          };

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

        services.flatpak.enable = true;

        # Better experience under high RAM pressure:
        # Use compressed RAM as swap
        zramSwap.enable = true;
        # earlyoom handles OOM more gracefully than systemd-oomd.
        # might just need to find the right systemd-oomd configuration.
        services.earlyoom = {
          enable = true;
          freeMemThreshold = 7;
          freeSwapThreshold = 50;
          enableNotifications = true;
        };
        # https://github.com/NixOS/nixpkgs/issues/374959
        systemd.services.earlyoom.serviceConfig.DynamicUser = false;
        systemd.oomd.enable = false;

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

        # Required for fx_cast, and possibly geoclue
        services.avahi = {
          enable = true;
          nssmdns4 = true;
        };

        # Set the system timezone automatically
        services.automatic-timezoned.enable = true;
      };
    };

  # Linux-only home-manager configuration shared across personal computers.
  flake.homeModules.linux-pc =
    {
      config,
      pkgs,
      ...
    }:
    {
      imports = [
        ../common_home.nix
        ../applications/gui.nix
        ../desktops/gnome
        ../desktops/wlroots
        ../peripherals/logitech.nix
      ];

      # Home Manager needs a bit of information about you and the
      # paths it should manage.
      home.homeDirectory = "/home/${config.home.username}";
      fonts.fontconfig.enable = true;
      targets.genericLinux.enable = true;

      home.packages = with pkgs; [
        # Programming languages
        gcc # required to compile some packages, e.g. emacsqlite

        # system tools
        dmidecode
        wireplumber
        appimage-run
        usbutils
        pciutils
      ];

      # Set up the 1password SSH agent
      # TODO: fix
      # programs.ssh.extraConfig = "IdentityAgent ~/.1password/agent.sock";

      # Services
      services.keybase.enable = true;

      programs.gpg.enable = true;

      services.gpg-agent.enable = true;
      # Source: https://discourse.nixos.org/t/cant-get-gnupg-to-work-no-pinentry/15373/2
      services.gpg-agent.pinentryPackage = pkgs.pinentry-gnome3;

      # Automatically start/stop/restart services when their configuration changes
      systemd.user.startServices = "sd-switch";

      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      home.stateVersion = "22.05";
    };
}
