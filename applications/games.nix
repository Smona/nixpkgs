{ config, lib, pkgs, ... }:

lib.mkIf (config.roles.gaming && config.graphical) {
  home.packages = with pkgs; [ prismlauncher jdk17 ];
}
