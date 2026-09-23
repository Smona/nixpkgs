# Module for enabling embedded development
{ ... }:

{
  flake.nixosModules.embedded-dev = { config, ... }: {
    # Set up sudoless access to MCUs
    users.groups =  { plugdev = {}; };
    services.udev.extraRules = builtins.readFile ./69-probe-rs.rules;
    users.users.${config.smona.username}.extraGroups = [
      "plugdev"
    ];
  };
}
