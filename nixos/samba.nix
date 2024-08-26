{ config, lib, pkgs, ... }:

let
  lilnasx_mount = { path }: {
    device = "//192.168.0.198/${path}";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

    in [
      "${automount_opts},credentials=/etc/nixos/smb-secrets,uid=1000,gid=100"
    ];
  };
in {
  # For mount.cifs, required unless domain name resolution is not needed.
  environment.systemPackages = [ pkgs.cifs-utils ];
  fileSystems."/mnt/share" = lilnasx_mount { path = "home"; };
  fileSystems."/mnt/media" = lilnasx_mount { path = "data/media"; };
}
