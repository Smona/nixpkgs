# Linux-only home-manager configurations.

{
  config,
  pkgs,
  inputs,
  system,
  ...
}:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.homeDirectory = "/home/${config.home.username}";
  fonts.fontconfig.enable = true;
  targets.genericLinux.enable = true;

  imports = [
    ./common_home.nix
    ./desktops/gnome
    ./desktops/wlroots
    ./peripherals/logitech.nix
  ];

  home.packages = with pkgs; [
    # Programming languages
    gcc # required to compile some packages, e.g. emacsqlite
    faust

    ## Python
    python310 # Required for advanced treemacs features

    ## Rust
    cargo
    cargo-watch
    rustc

    # system tools
    dmidecode
    wireplumber
    appimage-run
    dig
    dua # disk usage analyzer

    # Universal dev tools
    docker
    docker-compose
    vulnix
    # Cloud administration
    terraform
    awscli2

    # Fun
    cowsay
    ponysay
    pridecat
    nyancat
    blahaj
  ];

  # Set up the 1password SSH agent
  programs.ssh.extraConfig = "IdentityAgent ~/.1password/agent.sock";

  # Services
  services.keybase.enable = true;

  programs.gpg.enable = true;

  services.gpg-agent.enable = true;
  # Source: https://discourse.nixos.org/t/cant-get-gnupg-to-work-no-pinentry/15373/2
  services.gpg-agent.pinentryPackage =
    if pkgs.stdenv.isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;

  # Automatically start/stop/restart services when their configuration changes
  systemd.user.startServices = "sd-switch";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";
}
