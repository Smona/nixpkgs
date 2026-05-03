# Home Manager configurations for the smona user.
{ ... }:
{
  # Linux-only home-manager configuration shared across personal computers.
  flake.homeModules.linux-pc =
    {
      config,
      pkgs,
      ...
    }:
    {
      imports = [
        ../common_home.nix
        ../applications/gui.nix
        ../desktops/gnome
        ../desktops/wlroots
        ../peripherals/logitech.nix
      ];

      # Home Manager needs a bit of information about you and the
      # paths it should manage.
      home.homeDirectory = "/home/${config.home.username}";
      fonts.fontconfig.enable = true;
      targets.genericLinux.enable = true;

      home.packages = with pkgs; [
        # Programming languages
        gcc # required to compile some packages, e.g. emacsqlite

        # system tools
        dmidecode
        wireplumber
        appimage-run
        usbutils
        pciutils
      ];

      # Set up the 1password SSH agent
      # TODO: fix
      # programs.ssh.extraConfig = "IdentityAgent ~/.1password/agent.sock";

      # Services
      services.keybase.enable = true;

      programs.gpg.enable = true;

      services.gpg-agent.enable = true;
      # Source: https://discourse.nixos.org/t/cant-get-gnupg-to-work-no-pinentry/15373/2
      services.gpg-agent.pinentryPackage = pkgs.pinentry-gnome3;

      # Automatically start/stop/restart services when their configuration changes
      systemd.user.startServices = "sd-switch";

      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      home.stateVersion = "22.05";
    };

  # Darwin-only home-manager configuration shared across personal computers.
  flake.homeModules.darwin-pc =
    { pkgs, ... }:
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
    };
}
