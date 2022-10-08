#!/usr/bin/env bash

set -e

# Add the home-manager channel

nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

# Make sure the installer picks up nix in either single- or multi-user installations.
# This causes a warning when running nix commands, so we only set it temporarily here.
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
nix-shell '<home-manager>' -A install

# Install doom emacs
if [ ! -d "~/.emacs.d" ]; then
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
fi

echo "\nLogging into Keybase & importing your keys."
echo "If asked for a password, enter your Keybase account password."
keybase login
keybase pgp export | gpg --import
# The password is your keybase account password
keybase pgp export --secret | gpg --allow-secret-key-import --import
