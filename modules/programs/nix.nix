# Configurations for nix and my workflows around it.

{ inputs, ... }: {
  den.aspects.nix = {
    homeManager =
      {
        pkgs,
        lib,
        config,
        ...
      }:
      {
        # Configuration of the nix CLI
        # https://nixos.org/manual/nix/stable/command-ref/conf-file.html
        nix.package = lib.mkDefault pkgs.nix;
        nix.settings = {
          # Massively reduce disk usage by hard linking any duplicate files
          # to a single location on disk.
          # FIXME: only enable this outside of nix-darwin & nixos.
          # broken on darwin: https://github.com/NixOS/nix/issues/7273
          # auto-optimise-store = true;
          # Enable nix command and nix flakes
          experimental-features = "nix-command flakes";
          # Always show failure traces
          show-trace = true;
          substituters = [
            "https://cache.nixos.org"
            "https://nix-community.cachix.org"
          ];
          trusted-public-keys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
          netrc-file = "${config.home.homeDirectory}/.config/nix/netrc";
        };

        # Enable nix CLI helper
        programs.nh = {
          enable = true;
          flake = "$HOME/.config/nixpkgs";
        };

        # other programs and scripts for working with nix & nix system configs
        home.packages = with pkgs; [
          cachix
          nix-tree

          (writeShellScriptBin "upgrade" (
            if pkgs.stdenv.hostPlatform.isDarwin then
              ''
                nh darwin switch --ask "$@"
              ''
            else
              ''
                set -euo pipefail

                # 1. Build the configuration without activating it (creates ./result)
                nh os build --diff=never "$@"
                echo

                # 2. Compare booted initrd (and kernel/modules) with the newly built derivation
                booted_boot_files="$(readlink -f /run/booted-system/{initrd,kernel,kernel-modules})"
                built_boot_files="$(readlink -f ./result/{initrd,kernel,kernel-modules})"

                # 3. Choose 'boot' if early-boot components changed, otherwise 'switch'
                # Capture the exit code so a rejected change (exit 1) doesn't abort
                # under `set -e` before cleanup, and is preserved when the script ends.
                status=0
                needs_reboot=0
                if [ "$booted_boot_files" != "$built_boot_files" ]; then
                  echo "Kernel or initrd changed; setting as next boot target..."
                  echo
                  nh os boot --ask --quiet --no-rom || status=$?
                  if [ "$status" -eq 0 ]; then
                    needs_reboot=1
                  fi
                else
                  echo "No kernel or initrd changes detected. Switching to the new configuration..."
                  echo
                  nh os switch --ask --quiet --no-rom || status=$?
                fi

                # Clean up the temporary result symlink
                rm -f ./result

                # Offer to reboot now that the new boot target is set and cleanup is done.
                # Default is yes (Y/n); falls back to no on EOF/non-interactive input.
                if [ "$needs_reboot" -eq 1 ]; then
                  echo
                  read -r -p "Reboot now to apply the new configuration? [Y/n] " reply || reply="n"
                  # Only an explicit y/yes (or Enter) reboots, so stray input never
                  # reboots by accident. Strip whitespace, match case-insensitively.
                  reply="''${reply//[[:space:]]/}"
                  shopt -s nocasematch
                  case "$reply" in
                    "" | y | yes)
                      echo "Rebooting..."
                      sudo reboot
                      ;;
                    *)
                      echo "Not rebooting. Please reboot soon!"
                      ;;
                  esac
                  shopt -u nocasematch
                fi

                exit "$status"
              ''
          ))
          (writeShellScriptBin "update" ''
            # TODO: commit after update successfully applied
            upgrade --update
          '')
          (writeShellScriptBin "clean" ''
            nh clean all --keep 20 --keep-since 4w --ask --no-direnv
          '')
        ];
      };

    os = { lib, config, ... }: {
      nix = {
        # This will add each flake input as a registry
        # To make nix3 commands consistent with your flake
        registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

        # This will additionally add your inputs to the system's legacy channels
        # Making legacy nix commands consistent as well, awesome!
        nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

        # Periodically optimise the store
        optimise.automatic = true;

        settings = {
          # Users allowed to use additional binary caches and unsigned NARs.
          trusted-users = [
            "root"
            config.smona.username
          ];

          # Enable using flakes
          experimental-features = [
            "nix-command"
            "flakes"
          ];
        };
      };
    };
  };
}
