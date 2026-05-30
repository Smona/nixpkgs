# Noctalia wlroots shell. https://noctalia.dev/
{ inputs, ... }:
{
  flake.homeModules.noctalia =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      systembus-notify =
        (import ./_systembus-notify.nix { inherit pkgs; }).packages.noctalia-systembus-notify;
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

        home.packages = with pkgs; [
          fd # required for file search plugin: https://noctalia.dev/plugins/file-search

          # TODO: declaratively add to firefox native extensions
          pywalfox-native # support firefox & thunderbird themes
        ];

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
          lockCommand = "noctalia-shell ipc call lockScreen lock";
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
    };

  flake.nixosModules.noctalia =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf (config.smona.desktop.shell == "noctalia") {
        # Support for calendar events in noctalia.
        services.gnome.evolution-data-server.enable = true;
      };
    };
}
