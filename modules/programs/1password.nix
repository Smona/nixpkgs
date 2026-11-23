# Installation & configuration of 1password password manager.
#
# Needs to be installed at the system level, via nixpkgs on NixOS
# and homebrew on darwin.
{ den, ... }:
{
  den.aspects._1password = {
    includes = [ (den.batteries.unfree [ "1password" ]) ];

    homeManager = {
      # TODO: get this working on darwin
      # FIXME: this is breaking 1password settings currently
      # can we instead seed a writeable file?
      # xdg.configFile."1password-settings" = {
      #   target = "1Password/settings/settings.json";
      #   text = ''
      #     {
      #       "app.double": false,
      #       "security.authenticatedUnlock.enabled": true,
      #       "sshAgent.enabled": true,
      #       "sshAgent.storeKeyTitles": true,
      #       "sshAgent.sshAuthorizatonModel": "application"
      #     }
      #   '';
      # };
    };

    hmLinux = { config, lib, ... }: {
      options._1passwordBinary = lib.mkOption {
        type = lib.types.str;
        default = "/usr/bin/env 1password";
      };

      config = {
        # 1password has to be installed at the system level to integrate with polkit
        # and support system authentication / ssh agent / browser extension integration.
        systemd.user.services."1password" = {
          Unit.Description = "1password manager GUI application.";
          Install.WantedBy = [
            "graphical-session.target"
            "hyprland-session.target"
            "sway-session.target"
          ];
          Service = {
            ExecStart = "${config._1passwordBinary} --silent";
            Restart = "on-failure";
            RestartSec = 2;
          };
        };

        # Set up the 1password SSH agent
        # TODO: fix
        # programs.ssh.extraConfig = "IdentityAgent ~/.1password/agent.sock";
      };
    };

    hmDarwin = {
      # Set up the 1password SSH agent
      programs.ssh.extraConfig = ''
        IdentityAgent "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
      '';
    };

    nixos = { user, ... }: {
      programs._1password-gui = {
        enable = true;
        polkitPolicyOwners = [ user.userName ];
      };
    };

    darwin.homebrew.casks = [ "1password" ];
  };
}
