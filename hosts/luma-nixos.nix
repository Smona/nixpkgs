# WSL installation
#
# This was my first NixOS machine, and I rarely use Windows these days,
# so it's gotten a bit out of date with my current best practices.
{ inputs, self, ... }:
{
  den.aspects.luma-nixos.nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      wsl.defaultUser = "smona";

      # TODO: remove option once using user argument everywhere
      smona.username = "smona";

      environment.systemPackages = with pkgs; [
        git
        vim
      ];

      programs.zsh.enable = true;
      users.defaultUserShell = pkgs.zsh;

      programs._1password-gui = {
        enable = true;
        polkitPolicyOwners = [ "smona" ];
      };

      users.users.smona = {
        extraGroups = [ "wheel" ];
        uid = 1000;
      };
      home-manager.users.smona =
        { pkgs, config, ... }:
        {
          dconf.enable = true;

          _1passwordBinary = "/mnt/c/Users/mason/AppData/Local/Microsoft/WindowsApps/wslg.exe -d NixOS -- ${pkgs._1password-gui}/bin/1password";

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
        };

      home-manager.useGlobalPkgs = true;
      home-manager.extraSpecialArgs = {
        inherit inputs;
        system = "x86_64-linux";
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
