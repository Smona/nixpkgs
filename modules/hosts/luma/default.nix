# Baremetal installation
{ inputs, self, ... }:
{
  flake.nixosConfigurations."luma" = inputs.nixpkgs.lib.nixosSystem {
    pkgs = self.legacyPackages.x86_64-linux;
    specialArgs = {
      inherit inputs;
    };
    modules = [
      self.nixosModules.luma
    ];
  };

  flake.nixosModules.luma =
    {
      config,
      pkgs,
      inputs,
      ...
    }:
    {
      imports = [
        # Include the results of the hardware scan.
        ./_hardware-configuration.nix
        ./_containers.nix
        ../../../nixos/common_configuration.nix
        ../../../nixos/dualboot.nix
        ../../../nixos/graphics/amd.nix
        ../../../nixos/cpu/amd.nix
      ];

      smona.username = "mel";
      smona.primaryMonitor = "DP-1";
      smona.wallpaper = ../../../wallpapers/vibrant-paint-streaks.jpg;
      networking.hostName = "luma"; # Define your hostname.

      home-manager.users.mel =
        { pkgs, ... }:
        {
          imports = [ ../../../home.nix ];

          home.username = "mel";
          smona.wlroots.enable = true;
          smona.wlroots.primaryMonitor = config.smona.primaryMonitor;
          smona.wlroots.wallpaper = config.smona.wallpaper;
          # gnome.enable = true;
          roles = {
            gaming = true;
            music = true;
          };
          logitech.enabled = true;
          # Should apply to any NixOS machine
          _1passwordBinary = "${pkgs._1password-gui}/bin/1password";
        };

      # List services that you want to enable:

      services.hardware.openrgb = {
        enable = true;
        package = pkgs.openrgb-with-all-plugins;
      };
      programs.coolercontrol.enable = true;
      environment.systemPackages = with pkgs; [
        liquidctl
      ];

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "22.05"; # Did you read the comment?
    };
}
