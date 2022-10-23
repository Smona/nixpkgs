{ pkgs, lib, config, ... }:

{
  options.gnome.theme = lib.mkOption {
    type = lib.types.str;
    default = "flat-remix";
    description = "One of the themes from ./themes.nix.";
  };

  config = lib.mkIf config.gnome.enable (lib.mkMerge [
    (lib.mkIf (config.gnome.theme == "flat-remix") {
      gtk.theme = {
        package = pkgs.flat-remix-gtk;
        name = "Flat-Remix-GTK-Blue-Dark-Solid";
      };
      gtk.iconTheme = {
        package = pkgs.flat-remix-icon-theme;
        name = "Flat-Remix-Blue-Dark";
      };
      dconf.settings = {
        "org/gnome/shell/extensions/user-theme" = {
          name = "Flat-Remix-Blue-Dark-fullPanel";
        };
        "org/gnome/shell/extensions/unite" = {
          window-buttons-theme = "flat-remix";
        };
      };
      home.packages = [ pkgs.flat-remix-gnome ];
    })

    (lib.mkIf (config.gnome.theme == "dracula") {
      gtk.theme = {
        package = pkgs.dracula-theme;
        name = "Dracula";
      };
      dconf.settings = {
        "org/gnome/shell/extensions/user-theme" = { name = "Dracula"; };
        "org/gnome/shell/extensions/unite" = {
          window-buttons-theme = "dracula";
        };
      };
    })
  ]);
}
