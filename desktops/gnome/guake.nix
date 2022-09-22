{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.guake ];

  dconf.enable = true;
  dconf.settings = {
    "apps/guake/general" = { infinite-history = true; };
    "apps/guake/style/background".transparency = 90;
    "apps/guake/style/font" = {
      allow-bold = true;
      palette-name = "Dracula";
    };
  };
}
