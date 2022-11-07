{ config, lib, pkgs, ... }:

let cfg = config.smona.eww;
in {
  options.smona.eww = { enable = lib.mkEnableOption "eww widgets"; };

  config = lib.mkIf cfg.enable {
    programs.eww = {
      enable = true;
      package = pkgs.eww-wayland;
      configDir = ./eww;
    };
  };
}
