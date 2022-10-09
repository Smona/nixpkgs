rec {
  # Base configuration
  hostName = "choose_a_hostname";
  username = "smona";
  desktops = {
    gnome = {
      enable = false;
      theme = "flat-remix";
    };
  };
  roles = {
    gaming = true;
    work = false;
  };

  # Overrideable defaults (the default should work most of the time)
  homeDirectory = "/home/${username}";
  nixOS = builtins.pathExists /etc/nixos/configuration.nix;
}
