# Module for enabling embedded development
{ ... }:

{
  den.aspects.embedded-dev = {
    nixos = {user, ...}: {
      # Set up sudoless access to MCUs
      users.groups.plugdev = { };
      users.users.${user.userName}.extraGroups = [ "plugdev" ];
      services.udev.extraRules = builtins.readFile ./69-probe-rs.rules;
    };
  };
}
