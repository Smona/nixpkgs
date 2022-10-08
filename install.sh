#!/usr/bin/env bash

set -e

# Add the home-manager channel

nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

# Make sure the installer picks up nix in either single- or multi-user installations.
# This causes a warning when running nix commands, so we only set it temporarily here.
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
nix-shell '<home-manager>' -A install

