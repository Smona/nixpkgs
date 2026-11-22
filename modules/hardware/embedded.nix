# Module for enabling embedded development
{ ... }:

{
  den.aspects.embedded-dev = {
    user.extraGroups = [ "plugdev" ];

    nixos = {
      # Set up sudoless access to MCUs
      users.groups.plugdev = { };
      services.udev.extraRules = builtins.readFile ./69-probe-rs.rules;
    };
  };
}
