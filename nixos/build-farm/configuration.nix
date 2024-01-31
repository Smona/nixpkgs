{ config, lib, pkgs, system, modulesPath, ... }: {

  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  nixpkgs = { hostPlatform = system; };

  services.gitlab-runner.enable = true;
  services.gitlab-runner.services = {
    # Recommended setup from https://nixos.wiki/wiki/Gitlab_runner
    nix = {
      # File should contain at least these two variables:
      # `CI_SERVER_URL`
      # `REGISTRATION_TOKEN`
      registrationConfigFile = "/var/keys/gitlab-runner-registration";
      dockerImage = "alpine";
      dockerVolumes = [
        "/nix/store:/nix/store:ro"
        "/nix/var/nix/db:/nix/var/nix/db:ro"
        "/nix/var/nix/daemon-socket:/nix/var/nix/daemon-socket:ro"
      ];
      dockerDisableCache = true;
      preBuildScript = pkgs.writeScript "setup-container" ''
        mkdir -p -m 0755 /nix/var/log/nix/drvs
        mkdir -p -m 0755 /nix/var/nix/gcroots
        mkdir -p -m 0755 /nix/var/nix/profiles
        mkdir -p -m 0755 /nix/var/nix/temproots
        mkdir -p -m 0755 /nix/var/nix/userpool
        mkdir -p -m 1777 /nix/var/nix/gcroots/per-user
        mkdir -p -m 1777 /nix/var/nix/profiles/per-user
        mkdir -p -m 0755 /nix/var/nix/profiles/per-user/root
        mkdir -p -m 0700 "$HOME/.nix-defexpr"

        . ${pkgs.nix}/etc/profile.d/nix.sh

        ${pkgs.nix}/bin/nix-env -i ${
          builtins.concatStringsSep " " (with pkgs; [ nix cacert git openssh ])
        }

        ${pkgs.nix}/bin/nix-channel --add https://nixos.org/channels/nixpkgs-unstable
        ${pkgs.nix}/bin/nix-channel --update nixpkgs
      '';
      environmentVariables = {
        ENV = "/etc/profile";
        USER = "root";
        NIX_REMOTE = "daemon";
        PATH =
          "/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin";
        NIX_SSL_CERT_FILE =
          "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt";
      };
      tagList = [ "nix" ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.09"; # Did you read the comment?
}
