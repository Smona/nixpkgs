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
  roles = { work = false; };

  # Overrideable defaults (the default should work most of the time)
  homeDirectory = "/home/${username}";
  nixOS = builtins.pathExists /etc/nixos/configuration.nix;
}
