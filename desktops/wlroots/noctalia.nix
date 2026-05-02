{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    inputs.noctalia.homeModules.default
  ];

  config = {
    home.packages = with pkgs; [
      # networkmanagerapplet # for VPN configuration
    ];
    programs.noctalia-shell = {
      enable = true;
      package = (
        inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
          calendarSupport = true;
        }
      );
    };
  };
}
