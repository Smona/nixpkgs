{ config, lib, pkgs, isNixOS, ... }:

let wrapWithNixGL = import ./nixGL.nix { inherit pkgs isNixOS; };
in {
  home.shellAliases = {
    # Convenience SSH alias for installing kitty terminfo on servers
    s = "kitty +kitten ssh";
  };

  # My current preferred terminal
  programs.kitty = {
    enable = true;
    theme = "Tokyo Night";
    settings = {
      font_family = "Cascadia Code";
      background_opacity = "0.9";
      font_size = "12";
      window_padding_width = "4";
      touch_scroll_multiplier = "2";
      tab_bar_style = "slant";
      wayland_titlebar_color = "background";
      enable_audio_bell = false;
      visual_bell_duration = "0.3";
      visual_bell_color = "#777";
      update_check_interval = "0";
    };
  };
  # Provide access to drivers so hardware acceleration works on non-NixOS
  xdg.desktopEntries.kitty = {
    # TODO: figure out how to inherit attributes from the base desktop item
    name = "kitty";
    genericName = "Terminal";
    exec = wrapWithNixGL "kitty";
    categories = [ "System" "TerminalEmulator" ];
    icon = "kitty";
    type = "Application";
  };

  # Alacritty seems nice, but it's lacking some features and kept crashing gnome-shell
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
  # xdg.desktopEntries.Alacritty = {
  #   # TODO: figure out how to inherit attributes from the base desktop item
  #   name = "Alacritty";
  #   genericName = "Terminal";
  #   exec = wrapWithNixGL "alacritty";
  #   categories = [ "System" "TerminalEmulator" ];
  #   icon = "Alacritty";
  #   type = "Application";
  # };

}
