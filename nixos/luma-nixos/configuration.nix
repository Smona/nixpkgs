# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

# NixOS-WSL specific options are documented on the NixOS-WSL repository:
# https://github.com/nix-community/NixOS-WSL

{ config, lib, pkgs, inputs, ... }:

{
  wsl.enable = true;
  wsl.defaultUser = "smona";

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  nixpkgs = {
    hostPlatform = "x86_64-linux";
    config.allowUnfree = true;
  };

  networking.hostName = "luma-nixos";

  environment.systemPackages = with pkgs; [ git vim ];

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "smona" ];
  };

  users.users.smona = {
    isNormalUser = true;
    description = "Mel Bourgeois";
    extraGroups = [ "wheel" ];
    uid = 1000;
  };
  home-manager.users.smona = { pkgs, config, ... }: {
    imports = [ ../../home.nix ];

    home.username = "smona";
    home.homeDirectory = "/home/smona";
    home.stateVersion = "22.05";

    dconf.enable = true;

    _1passwordBinary =
      "/mnt/c/Users/mason/AppData/Local/Microsoft/WindowsApps/wslg.exe -d NixOS -- ${pkgs._1password-gui}/bin/1password";

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
}
