{ config, lib, pkgs, ... }:

let cfg = config.smona.eww;
in {
  options.smona.eww = { enable = lib.mkEnableOption "eww widgets"; };

  config = lib.mkIf cfg.enable {
    home.packages = [ (import ./hypr_info { inherit pkgs; }) ];
    programs.eww = {
      enable = true;
      configDir = ./eww;
    };
  };
}
