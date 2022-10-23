#!/usr/bin/env sh

sudo nixos-rebuild --flake ".#$1" switch

./install.sh
