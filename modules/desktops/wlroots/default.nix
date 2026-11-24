# Shared configs for all wlroots compositors (Hyprland, Niri, Sway)

{ den, inputs, ... }: {
  den.aspects.wlroots = {
    includes = with den.aspects; [ waybar ];

    hmLinux =
      {
        host,
        config,
        lib,
        pkgs,
        ...
      }:

      let
        cmd = import ./_system-commands { inherit pkgs inputs; };
        commonOptions = import ./_common.nix { inherit pkgs inputs config; };
        cfg = config.smona.wlroots;
        inWlroots = builtins.elem host.desktop.compositor [
          "hyprland"
          "niri"
          "sway"
        ];
      in
      {
        imports = [ inputs.wayland-pipewire-idle-inhibit.homeModules.default ];

        options.smona.wlroots = with lib; {
          primaryMonitor = mkOption {
            description = "Which monitor ID represents the 'primary' monitor.";
            type = types.str;
          };
          execStart = mkOption {
            description = "Extra commands to spawn once when the wlroots session starts.";
            type = types.listOf types.str;
            default = [ ];
          };
          execAlways = mkOption {
            description = "Extra commands to spawn on every wlroots session reload.";
            type = types.listOf types.str;
            default = [ ];
          };
          sessionMenuCommand = mkOption {
            description = "Command spawned by Mod+Shift+e to open the session menu.";
            type = types.unique {
              message = "smona.wlroots.sessionMenuCommand may only be set by one module.";
            } (types.listOf types.str);
            default = [ ];
          };
          notificationsCommand = mkOption {
            description = "Command spawned by Mod+m to toggle the notifications panel.";
            type = types.unique {
              message = "smona.wlroots.notificationsCommand may only be set by one module.";
            } (types.listOf types.str);
            default = [ ];
          };
          lockCommand = mkOption {
            description = "Shell-specific lock command, will run after 1password --lock.";
            type = types.unique {
              message = "smona.wlroots.lockCommand may only be set by one module.";
            } types.str;
            default = "";
          };
          screenOnCommand = mkOption {
            description = "Shell-specific command to power monitors back on.";
            type = types.unique {
              message = "smona.wlroots.screenOnCommand may only be set by one module.";
            } types.str;
            default = "";
          };
          screenOffCommand = mkOption {
            description = "Shell-specific command to power monitors off.";
            type = types.unique {
              message = "smona.wlroots.screenOffCommand may only be set by one module.";
            } types.str;
            default = "";
          };
          keyBinds = mkOption {
            description = "Extra shell-specific keybindings to add to the wlroots session.";
            type = types.listOf (types.attrsOf types.anything);
            default = [ ];
          };
        };

        config = lib.mkIf inWlroots {
          home.packages = with pkgs; [
            inotify-tools
            wofi
            wl-clipboard
            # Needed for flameshot
            grim
            slurp
            playerctl
            rofimoji
            fusuma

            cmd.tao
          ];

          # Keeps track of media players so playerctl always acts on the most
          # recently active one.
          services.playerctld.enable = true;

          # inhibit idle lock when playing media, regardless of source
          # NOTE: this will inhibit locking when music is playing. This is a quick fix, but it
          # would be better to continue locking & screen blanking when music is playing, but provide
          # music controls on the lock screen and disable suspend instead. locking should still be fully
          # disabled when videos are playing.
          # TODO: move to custom-shell if confirmed unnecessary on noctalia
          services.wayland-pipewire-idle-inhibit = {
            enable = true;
            systemdTarget = "graphical.target";
            settings = {
              verbosity = "INFO";
              idle_inhibitor = "wayland";
            };
          };

          services.hypridle = {
            enable = true;
            settings = {
              general = {
                lock_cmd = commonOptions.lock;
                before_sleep_cmd = commonOptions.lock;
              };
              listener = [
                {
                  timeout = 240;
                  # Locking should come before screen off to prevent FOIC (flash of insecure content)
                  on-timeout = commonOptions.lock;
                }
                {
                  timeout = 480;
                  on-timeout = cfg.screenOffCommand;
                  on-resume = cfg.screenOnCommand;
                }
                # FIXME: re-enable on laptop but not desktop
                # {
                #   timeout = 600;
                #   on-timeout = "systemctl suspend-then-hibernate";
                # }
              ];
            };
          };
        };
      };

    nixos =
      {
        host,
        config,
        lib,
        pkgs,
        ...
      }:

      let
        inWlroots = builtins.elem host.desktop.compositor [
          "hyprland"
          "niri"
          "sway"
        ];
      in
      {
        config = lib.mkIf inWlroots {
          environment.sessionVariables = {
            # Force electron apps to run in wayland natively. This is required for
            # them to display on scaled monitors without getting blurry.
            ELECTRON_OZONE_PLATFORM_HINT = "wayland";
          };

          services.dbus.enable = true;
          # Location service provider, required for gammastep
          services.geoclue2.enable = true;

          hardware.bluetooth.enable = true;

          xdg.portal = {
            enable = true;
            # fix GTK theming in hyprland
            extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
            wlr.enable = true;
          };

          # Enable a keyring service and password UI for non-gnome environments
          # https://discourse.nixos.org/t/login-keyring-did-not-get-unlocked-hyprland/40869/8?u=smona
          services.gnome.gnome-keyring.enable = true;
          programs.seahorse.enable = true;
          security.pam.services.gdm-password.enableGnomeKeyring = true;

          # Auto-mount (and unmount) removable drives
          services.devmon.enable = true;
          services.gvfs.enable = true;
          services.udisks2.enable = true;
        };
      };
  };
}
