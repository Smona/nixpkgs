# Configuration for a system with an AMD GPU.
{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [ ./common.nix ];

  # Load AMD driver for Xorg and Wayland
  boot.initrd.kernelModules = [ "amdgpu" ];
  # fix occasional green artifacts in Hyprland:
  # https://github.com/hyprwm/Hyprland/issues/2385
  boot.kernelParams = [ "amdgpu.dcdebugmask=0x10" ];
  services.xserver.videoDrivers = [ "amdgpu" ];

  # Radeon GPU
  systemd.services.lact = {
    description = "AMDGPU Control Daemon";
    after = [ "multi-user.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.lact}/bin/lact daemon";
    };
    enable = true;
  };

  environment.systemPackages = [ pkgs.lact ];
}
