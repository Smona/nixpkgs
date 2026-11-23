# My standard user & system configurations
{
  inputs,
  den,
  ...
}:
{
  # alternate usernames
  den.aspects.mel.includes = [ den.aspects.smona ];
  den.aspects.smona = {
    user = {
      description = "Mel Bourgeois";
    };

    includes = with den.aspects; [
      den.batteries.primary-user

      gaming
      desktop
      gui

      fonts

      # shell config
      shells
      git
      vim
      tmux
      pi
      nix

      # apps
      vicinae
      emacs

      # peripherals
      keychron
      zsa
      lilnasx
      embedded-dev

      (den.batteries.unfree [ "1password" ])
    ];

    # Home Manager configuration shared between Linux & Darwin.
    # As much stuff as possible should go in here, but some things are
    # necessarily different between the two platforms.
    homeManager =
      {
        pkgs,
        lib,
        config,
        ...
      }:
      {
        imports = [ inputs.catppuccin.homeModules.catppuccin ];

        options = with lib; {
          pkgsCompat = mkOption {
            type = types.pkgs;
            default = pkgs;
            description = ''
              A version of nixpkgs which lags behind the main package set, to preserve
              compatibility with non-NixOS installs on distributions that have slower
              release cycles (e.g. Ubuntu).

              This is especially necessary for packages which integrate closely with the
              system-supplied desktop environment (e.g. gnome extensions), but can come
              up in other cases as well.
            '';
          };
        };

        config = {
          catppuccin.enable = true;
          catppuccin.flavor = "mocha";

          home.language.base = "en_US.UTF-8";

          home.packages = with pkgs; [
            cachix

            ## NodeJS
            nodejs
            yarn
            prettier
          ];

          # Set vim as default editor for quick stuff
          home.sessionVariables =
            let
              editor = "vim";
            in
            {
              VISUAL = editor;
              EDITOR = editor;
              GIT_EDITOR = editor;
            };

          programs.ssh = {
            enable = true;
            includes = [ "~/.ssh/config.local" ];
          };

          # Services
          services.syncthing.enable = true;

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
        };
      };

    # Linux-only home-manager configuration shared across personal computers.
    hmLinux = { config, pkgs, ... }: {
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
      services.gpg-agent.pinentry.package = pkgs.pinentry-gnome3;

      # Automatically start/stop/restart services when their configuration changes
      systemd.user.startServices = "sd-switch";
    };

    # Darwin-only home-manager configuration shared across personal computers.
    hmDarwin =
      { pkgs, ... }:
      {
        graphical = true;
        roles = {
          work = true;
        };

        home.packages = with pkgs; [
          colima
          vlc-bin
        ];

        # Set up the 1password SSH agent
        programs.ssh.extraConfig = ''
          IdentityAgent "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
        '';

        # You should not change this value, even if you update Home Manager. If you do
        # want to update the value, then make sure to first check the Home Manager
        # release notes.
        home.stateVersion = "24.05"; # Please read the comment before changing.
      };

    # System configuration that can be shared between nix-darwin & NixOS.
    os =
      {
        config,
        lib,
        pkgs,
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
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          # Automatically backup existing files
          home-manager.backupFileExtension = "bk";

          # Configure the system-wide /etc/zshrc, and install zsh for root on linux.
          # Note that this is required on darwin, since it loads the nix-darwin environment in the default shell on macos.
          programs.zsh.enable = true;

          time.timeZone = lib.mkDefault "America/Denver";

          environment.systemPackages = with pkgs; [
            git
            vim
          ];
        };
      };

    nixos =
      {
        config,
        pkgs,
        lib,
        user,
        ...
      }:
      {
        imports = [ inputs.catppuccin.nixosModules.catppuccin ];

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
            wifi.backend = "iwd";
            plugins = with pkgs; [ networkmanager-openvpn ];
          };

          # IWD is much quicker to connect IME.
          networking.wireless.iwd = {
            enable = true;
            # Fixes an issue where luma would randomly disconnect from home wifi
            # by roaming to nowhere.
            settings.Scan.DisableRoamingScan = true;

            settings.General = {
              EnableNetworkConfiguration = true;
              # Another option: roam less aggressively
              # RoamThreshold=-74;
              # RoamThreshold5G=-79;
            };
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
          ];
          # Enable file previews for nautilus with spacebar.
          services.gnome.sushi.enable = true;

          # Firmware
          services.fwupd.enable = true; # Firmware update manager
          # Make sure we have the best firmware available.
          # This is especially important for dual-booting, since firmware installed in
          # Windows can break bluetooth or other features
          hardware.enableAllFirmware = true;

          # Define a user account. Don't forget to set a password with ‘passwd’.
          users.users.${user.userName} = {
            extraGroups = [
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

          programs.kdeconnect.enable = true;

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
  };
}
