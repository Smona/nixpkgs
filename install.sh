#!/usr/bin/env bash
# Installs the user environment.
# Make sure the user is set up in flake.nix with the proper user@hostname key first.

set -e


# Install doom emacs
if [ ! -d "$HOME/.emacs.d" ]; then
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
fi

echo
echo "Logging into Keybase & importing your keys."
echo "If asked for a password, enter your Keybase account password."
keybase login
keybase pgp export | gpg --import
# The password is your keybase account password
keybase pgp export --secret | gpg --allow-secret-key-import --import

