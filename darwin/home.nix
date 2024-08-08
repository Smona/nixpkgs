{ config, pkgs, ... }:

{
  imports = [ ../common_home.nix ../applications/gui.nix ];

  graphical = true;
  programs.firefox.package = null;

  # NOTE: most of this should be merged with my tracked configs, just getting things working for now

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
        IdentityAgent "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
    '';
  };

  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.
}
