{
  flat-remix = { pkgs, ... }: {
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
  };
  dracula = { pkgs, ... }: {
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
  };
}
