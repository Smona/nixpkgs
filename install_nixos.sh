#!/usr/bin/env sh

set -e

sudo nixos-rebuild --flake ".#$1" boot

./install.sh
