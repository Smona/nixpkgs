{ ... }:

{
  # Alacritty seems nice, but last i tried it, it was lacking some features and
  # kept crashing gnome-shell
  den.aspects.alacritty.homeManager = { config, pkgs, ... }: {
    programs.alacritty = {
      enable = true;
      package = (config.lib.nixGL.wrap pkgs.alacritty);
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
        font.size = 12.0;
        cursor = {
          style.shape = "Beam";
          vi_mode_style.shape = "Block";
        };
      };
    };
  };
}
