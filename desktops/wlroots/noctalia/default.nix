{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  systembus-notify = (import ./systembus-notify.nix { inherit pkgs; }).packages.noctalia-systembus-notify;
in
{
  imports = [
    inputs.noctalia.homeModules.default
  ];

  config = lib.mkIf (config.smona.desktop.shell == "noctalia") {
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

    smona.wlroots.execStart = [
      "${systembus-notify}/bin/noctalia-systembus-notify"
      "noctalia-shell"
    ];
  };
}
