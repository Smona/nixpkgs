{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.smona.eww;
  eww = inputs.my-nixpkgs.legacyPackages.x86_64-linux.eww-wayland;
in {
  options.smona.eww = { enable = lib.mkEnableOption "eww widgets"; };

  config = lib.mkIf cfg.enable {
    home.packages = [ (import ./hypr_info { inherit pkgs; }) ];
    programs.eww = {
      enable = true;
      package = eww;
      configDir = ./eww;
    };
  };
}
