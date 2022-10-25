{ config, lib, pkgs, ... }:

lib.mkIf config.roles.gaming { home.packages = with pkgs; [ prismlauncher ]; }
