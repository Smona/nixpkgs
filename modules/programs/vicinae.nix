# Configuration for Vicinae, the true cross-platform everything launcher

{ inputs, ... }:
{
  flake.homeModules.vicinae =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      isNoctalia = config.smona.desktop.shell == "noctalia";
    in
    {
      imports = [ inputs.vicinae.homeManagerModules.default ];

      catppuccin.vicinae.enable = lib.mkForce (!isNoctalia);
      programs.vicinae = {
        enable = true;
        systemd = {
          enable = true;
          autoStart = true;
          environment = {
            USE_LAYER_SHELL = 1;
          };
        };
        settings = {
          theme = (
            lib.mkIf isNoctalia {
              light = {
                name = "noctalia";
                icon_theme = "default";
              };
              dark = {
                name = "noctalia";
                icon_theme = "default";
              };
            }
          );
        };
      };
    };

  flake.nixosModules.vicinae =
    { ... }:
    {
      imports = [ inputs.vicinae.nixosModules.default ];
    };
}
