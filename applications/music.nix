{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.roles.music {
    home.packages = with pkgs; [
      bitwig-studio
      reaper
      real_time_config_quick_scan
      qpwgraph
    ];
  };
}
