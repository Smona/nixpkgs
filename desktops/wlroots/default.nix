{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.smona.wlroots;
  cmd = import ./system-commands { inherit pkgs inputs; };
  my_rofi = pkgs.rofi-wayland.override { plugins = with pkgs; [ rofi-calc ]; };
in {
  imports =
    [ ../../applications/gui.nix ./waybar ./eww.nix ./sway.nix ./hyprland.nix ];

  options.smona.wlroots = {
    enable = lib.mkEnableOption "wlroots window managers";
  };

  config = lib.mkIf cfg.enable {
    graphical = true;

    smona.waybar.enable = true;
    smona.eww.enable = true;

    home.packages = with pkgs; [
      inotify-tools
      wofi
      swaybg
      # Needed for flameshot
      grim
      slurp
      swaylock-effects
      playerctl
      (rofimoji.override { rofi = my_rofi; })
      fusuma
      blueman # GTK bluetooth manager
    ];

    services.kanshi = {
      enable = true;
      profiles = {
        undocked = { outputs = [{ criteria = "eDP-1"; }]; };
        home-office = {
          outputs = [
            { criteria = "eDP-1"; }
            {
              criteria = "Acer Technologies Acer XR382CQK 0x0000B7AA";
              position = "-1820,-1600";
            }
          ];
        };
      };
    };

    services.flameshot = {
      enable = true;
      # settings = {};
    };

    programs.rofi = {
      enable = true;
      package = my_rofi;
      terminal = "${pkgs.kitty}/bin/kitty";
      theme = "arthur";
    };

    # Keeps track of media players so playerctl always acts on the most
    # recently active one.
    services.playerctld.enable = true;

    services.swayidle = {
      enable = true;
      systemdTarget = "sway-session.target hyprland-session.target";
      events = [{
        event = "before-sleep";
        command = cmd.goodbye;
      }];
      timeouts = [
        {
          timeout = 240;
          # Locking should come before screen off to prevent FOIC (flash of insecure content)
          command = builtins.toString cmd.lock;
        }
        {
          timeout = 270;
          command = builtins.toString cmd.screenOff;
          resumeCommand = builtins.toString cmd.screenOn;
        }
        {
          timeout = 600;
          command = "systemctl suspend-then-hibernate";
        }
      ];
    };

    wayland.windowManager.sway.enable = true;
    wayland.windowManager.hyprland.enable = true;

    services.fusuma = {
      enable = true;
      extraPackages = with pkgs; [ my_rofi coreutils-full wtype ];
      settings = {
        threshold = { swipe = 0.1; };
        interval = { swipe = 0.7; };
        swipe = {
          "4" = {
            left = { command = "${pkgs.sway}/bin/swaymsg workspace next"; };
            right = { command = "${pkgs.sway}/bin/swaymsg workspace prev"; };
          };
        };
      };
    };
  };
}
