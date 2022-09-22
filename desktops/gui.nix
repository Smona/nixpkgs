{ config, lib, pkgs, ... }:

let
  settings = import ../settings.nix;
  wrapWithNixGL = import ../applications/nixGL.nix pkgs;
in {
  imports = [ ../applications/firefox.nix ];

  home.packages = with pkgs;
    [
      # Graphical applications
      # FIXME: system authentication isn't working, preventing use of SSH agent
      # _1password-gui
      discord
      rescuetime
      signal-desktop
      slack
      spotify
    ] ++ (lib.lists.optional settings.roles.work zoom-us);

  programs.chromium.enable = true;

  programs.kitty = {
    enable = true;
    settings = { background_opacity = "0.9"; };
  };
  # Provide access to drivers so hardware acceleration works on non-NixOS
  xdg.desktopEntries.kitty = {
    # TODO: figure out how to inherit attributes from the base desktop item
    name = "kitty";
    genericName = "Terminal";
    exec = wrapWithNixGL "kitty";
    categories = [ "System" "TerminalEmulator" ];
    icon = "Kitty";
    type = "Application";
  };

  programs.alacritty = {
    enable = false;
    settings = {
      window = {
        decorations = "none";
        opacity = 0.9;
        padding = {
          x = 14;
          y = 7;
        };
        position = {
          x = 0;
          y = 0;
        };
      };
      font = { size = 12.0; };
      cursor = {
        style = { shape = "Beam"; };
        vi_mode_style = { shape = "Block"; };
      };
    };
  };
  # Provide access to drivers so hardware acceleration works on non-NixOS
  xdg.desktopEntries.Alacritty = {
    # TODO: figure out how to inherit attributes from the base desktop item
    name = "Alacritty";
    genericName = "Terminal";
    exec = wrapWithNixGL "alacritty";
    categories = [ "System" "TerminalEmulator" ];
    icon = "Alacritty";
    type = "Application";
  };

  systemd.user.services.rescuetime = {
    Unit.Description = "RescueTime time tracker";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/rescuetime";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
  # systemd.user.services."1password" = {
  #   Unit.Description = "1password manager GUI application.";
  #   Install.WantedBy = [ "graphical-session.target" ];
  #   Service = {
  #     ExecStart = "${settings.homeDirectory}/.nix-profile/bin/1password";
  #     Restart = "on-failure";
  #     RestartSec = 2;
  #   };
  # };

  # I like to have slack installed everywhere, but only auto-start it on work machines
  systemd.user.services.slack = lib.mkIf settings.roles.work {
    Unit.Description = "Slack desktop application";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${settings.homeDirectory}/.nix-profile/bin/slack";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };
}
