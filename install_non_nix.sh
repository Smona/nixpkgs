#!/usr/bin/env bash

set -e

# Install Nix (single user mode)
# NOTE: if installing on a system multiple people will be using, manually
#       install the multi-user installation instead.
# See: https://nixos.org/manual/nix/stable/installation/installing-binary.html#single-user-installation
sh <(curl -L https://nixos.org/nix/install) --no-daemon
. $HOME/.nix-profile/etc/profile.d/nix.sh

./install.sh

echo 'Adding you to the "docker" group...'
sudo usermod -aG docker $USER
