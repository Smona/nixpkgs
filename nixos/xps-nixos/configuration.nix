# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  inputs,
  system,
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan.
    ../common_configuration.nix
    inputs.hardware.nixosModules.dell-xps-13-7390
  ];

  smona.username = "smona";
  smona.primaryMonitor = "eDP-1";
  smona.wallpaper = ../../wallpapers/neon-highway-wallpaper.jpg;
  networking.hostName = "xps-nixos"; # Define your hostname.

  # Always use Cloudflare nameservers
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
  ];

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  home-manager.users.smona =
    { pkgs, ... }:
    {
      imports = [ ../../home.nix ];

      home.username = "smona";
      smona.wlroots = {
        enable = true;
        builtInDisplay = "eDP-1";
        primaryMonitor = config.smona.primaryMonitor;
        wallpaper = config.smona.wallpaper;
      };
      roles = {
        art = true;
        gaming = true;
        music = true;
      };
      logitech.enabled = true;
      # Should apply to any NixOS machine
      _1passwordBinary = "${pkgs._1password-gui}/bin/1password";
    };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "smona" ];
  virtualisation.docker.enable = true;

  # Laptop stuff
  services.power-profiles-daemon.enable = false;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
    };
  };

  # Enable IIO for autorotation and light detection
  hardware.sensor.iio.enable = true;

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=4h
    # https://github.com/NixOS/nixos-hardware/tree/master/dell/xps/13-7390
    SuspendState=freeze
  '';
  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    lidSwitchExternalPower = "suspend-then-hibernate";
    extraConfig = ''
      HandlePowerKey=suspend-then-hibernate
    '';
  };

  services.flatpak.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
