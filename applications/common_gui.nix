# Configs shared between graphical linux installs and darwin

{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  my-slack = (config.lib.nixGL.wrap pkgs.slack);
in
{
  imports = [
    ./firefox.nix
    ./terminal.nix
    ./thunderbird
    inputs.spicetify-nix.homeManagerModules.default
  ];

  options = with lib; {
    graphical = mkEnableOption "installing and configuring graphical applications";

    # High-level toggles for different tasks i want to do on each computer.
    # They're all defined here for ease of discoverability
    roles = {
      art = mkEnableOption "set up computer for visual art creation";
      gaming = mkEnableOption "set up computer for gaming";
      work = mkEnableOption "set up computer for work";
      music = mkEnableOption "set up computer for music production";
    };
  };

  config = lib.mkIf config.graphical {
    programs.kitty.enable = true;
    # programs.alacritty.enable = true;

    home.packages = with pkgs; [
      # Messaging apps
      (config.lib.nixGL.wrap (
        if pkgs.stdenv.isDarwin then
          discord
        else
          discord.override {
            nss = nss_latest; # https://github.com/NixOS/nixpkgs/issues/78961
          }
      ))
      my-slack

      # Media apps
      # TODO: restore nixGL?
      # (nixGL spotify)
      # TODO: upgrade once gimp3 is supported on darwin.
      (config.lib.nixGL.wrap gimp2)
    ];

    programs.spicetify =
      let
        spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
      in
      {
        enable = true;
        # TODO: check out spicetify extensions
        enabledExtensions = with spicePkgs.extensions; [
          #   hidePodcasts
          shuffle # shuffle+ (special characters are sanitized out of extension names)
          fullAppDisplay
        ];
        theme = spicePkgs.themes.catppuccin;
        colorScheme = config.catppuccin.flavor;
      };

    # I like to have slack installed everywhere, but only auto-start it on work machines
    systemd.user.services.slack = lib.mkIf config.roles.work {
      Unit.Description = "Slack desktop application";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${my-slack}/bin/slack";
        Restart = "on-failure";
        RestartSec = 2;
      };
    };
  };
}
