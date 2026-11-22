# WSL installation
#
# This was my first NixOS machine!
{ ... }:
{
  den.aspects.luma-nixos.nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      # TODO: remove option once using user argument everywhere
      smona.username = "smona";

      # Not sure why this is here
      users.users.smona.uid = 1000;

      home-manager.users.smona =
        { pkgs, ... }:
        {
          dconf.enable = true;
          _1passwordBinary = "/mnt/c/Users/mason/AppData/Local/Microsoft/WindowsApps/wslg.exe -d NixOS -- ${pkgs._1password-gui}/bin/1password";
        };

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It's perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "23.05"; # Did you read the comment?
    };
}
