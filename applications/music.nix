{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.roles.music {
    home.packages = with pkgs;
      [ real_time_config_quick_scan ]
      ++ (lib.lists.optionals config.graphical [ bitwig-studio qpwgraph ]);
  };
}
