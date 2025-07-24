# Darwin-only home-manager configurations

{ config, pkgs, ... }:

{
  imports = [ ../common_home.nix ];

  graphical = true;
  roles = {
    work = true;
  };

  home.packages = with pkgs; [
    colima
    vlc-bin
  ];

  # Set up the 1password SSH agent
  programs.ssh.extraConfig = ''
    IdentityAgent "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
  '';

  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.
}
