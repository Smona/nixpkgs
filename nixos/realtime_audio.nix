{ inputs, lib, pkgs, ... }:

{
  imports = [ inputs.musnix.nixosModules.musnix ];

  # https://github.com/musnix/musnix#base-options
  musnix.enable = true;
  users.users.smona.extraGroups = [ "audio" ];

  # Not currently working on my nixpkgs version
  # error: attribute 'linuxPackages_5_4_rt' missing
  # musnix.kernel.realtime = true;

  services.pipewire.jack.enable = true;

  environment.systemPackages = with pkgs; [ qjackctl ];
}
