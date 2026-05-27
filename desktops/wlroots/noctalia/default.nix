{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  systembus-notify =
    (import ./systembus-notify.nix { inherit pkgs; }).packages.noctalia-systembus-notify;
in
{
  imports = [
    inputs.noctalia.homeModules.default
  ];

  config = lib.mkIf (config.smona.desktop.shell == "noctalia") {
    programs.noctalia-shell = {
      enable = true;
      package = (
        inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
          calendarSupport = true;
        }
      );
    };

    smona.wlroots = {
      execStart = [
        "${systembus-notify}/bin/noctalia-systembus-notify"
        "noctalia-shell"
      ];
      sessionMenuCommand = [
        "noctalia-shell"
        "ipc"
        "call"
        "sessionMenu"
        "toggle"
      ];
      launcherCommand = [
        "noctalia-shell"
        "ipc"
        "call"
        "launcher"
        "toggle"
      ];
      notificationsCommand = [
        "noctalia-shell"
        "ipc"
        "call"
        "notifications"
        "toggleHistory"
      ];
      keyBinds = [
        {
          primaryMod = true;
          key = "s";
          command = [
            "noctalia-shell"
            "ipc"
            "call"
            "settings"
            "toggle"
          ];
        }
      ];
    };
  };
}
