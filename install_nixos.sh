#!/usr/bin/env sh

set -e

if [ ! -f /etc/nixos/smb-secrets ]; then
  echo
  echo "Setting up NAS mount."
  read -p "NAS username: " NAS_USER
  read -sp "NAS password: " NAS_PASS
  echo
  echo -e "username=$NAS_USER\npassword=$NAS_PASS" | sudo tee /etc/nixos/smb-secrets >/dev/null
  echo "Successfully wrote samba credentials to /etc/nixos/smb-secrets"
fi

sudo nixos-rebuild --flake ".#$1" boot

./install.sh
