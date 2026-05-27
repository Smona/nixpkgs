{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cmd = import ./system-commands { inherit pkgs inputs; };
  inWlroots = builtins.elem config.smona.desktop.compositor [
    "hyprland"
    "niri"
    "sway"
  ];
in
{
  imports = [
    inputs.wayland-pipewire-idle-inhibit.homeModules.default

    ./niri.nix
    ./sway.nix
    ./hyprland.nix
    ./noctalia
    ./waybar
    ./custom-shell.nix
  ];

  options.smona.wlroots = with lib; {
    builtInDisplay = mkOption {
      description = "Which monitor ID represents the builtin screen. Get the ID via `swaymsg -t get_outputs`";
      type = types.str;
      default = "";
    };
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
    launcherCommand = mkOption {
      description = "Command spawned by Alt+space to open the application launcher.";
      type = types.unique {
        message = "smona.wlroots.launcherCommand may only be set by one module.";
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
    keyBinds = mkOption {
      description = "Extra shell-specific keybindings to add to the wlroots session.";
      type = types.listOf (types.attrsOf types.anything);
      default = [ ];
    };
  };

  config = lib.mkIf inWlroots {
    graphical = true;

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

      rot8
      cmd.tao
    ];

    # Keeps track of media players so playerctl always acts on the most
    # recently active one.
    services.playerctld.enable = true;

    # inhibit idle lock when playing media
    # NOTE: this will inhibit locking when music is playing. This is a quick fix, but it
    # would be better to continue locking & screen blanking when music is playing, but provide
    # music controls on the lock screen and disable suspend instead. locking should still be fully
    # disabled when videos are playing.
    services.wayland-pipewire-idle-inhibit = {
      enable = true;
      systemdTarget = "hyprland-session.target";
      settings = {
        verbosity = "INFO";
        idle_inhibitor = "wayland";
      };
    };

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = cmd.lock;
          before_sleep_cmd = "${cmd.lock} --immediate";
        };
        listener = [
          {
            timeout = 240;
            # Locking should come before screen off to prevent FOIC (flash of insecure content)
            on-timeout = cmd.lock;
          }
          {
            timeout = 480;
            on-timeout = builtins.toString cmd.screenOff;
            on-resume = builtins.toString cmd.screenOn;
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

}
