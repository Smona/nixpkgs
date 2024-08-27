{ pkgs, ... }:

{
  uiFont = {
    name = "Source Sans 3";
    size = 14;
    package = pkgs.source-sans;
  };
  gtk = {
    package = pkgs.flat-remix-gtk;
    name = "Flat-Remix-GTK-Blue-Dark-Solid";
  };
  icons = {
    package = pkgs.flat-remix-icon-theme;
    name = "Flat-Remix-Blue-Dark";
  };
  cursor = {
    package = pkgs.dracula-theme;
    name = "Dracula-cursors";
  };
  cursorSize = 24;
  gnome = {
    package = pkgs.flat-remix-gnome;
    shell = "Flat-Remix-Blue-Dark-fullPanel";
    uniteButtons = "flat-remix";
  };
}

# inactive themes
# {
#   gtk = {
#     package = pkgs.dracula-theme;
#     name = "Dracula";
#   };
#   gnome = {
#     extensions = "Dracula";
#     uniteButtons = "dracula";
#   };
# }
