#!/usr/bin/env bash

set -e

# Install Nix (single user mode)
# NOTE: if installing on a system multiple people will be using, manually
#       install the multi-user installation.
# See: https://nixos.org/manual/nix/stable/installation/installing-binary.html#single-user-installation

sh <(curl -L https://nixos.org/nix/install) --no-daemon

# Add the home-manager channel

nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

# Clean up dotfiles that have already been migrated to home-manager
#
# TODO: make this safer for anyone who stumbles across it. It's safe
# for me because all these paths are just symbolic links to another repo.

rm -rf ~/.ackrc ~/.curlrc ~/.gemrc ~/.screenrc ~/.doom.d ~/.zpreztorc ~/.zshrc \
      ~/.gitignore ~/.gitattributes ~/.gitconfig ~/.tmux.conf ~/.inputrc ~/.zshenv

# Make sure the installer picks up nix in either single- or multi-user installations.
# This causes a warning when running nix commands, so we only set it temporarily here.

export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
nix-shell '<home-manager>' -A install

echo 'Adding you to the "docker" group...'
sudo usermod -aG docker $USER
