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
    ./noctalia.nix
    ./waybar
    # ./custom-shell.nix
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

      (import ./tablet_mode_switch { inherit pkgs; })
      squeekboard
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
