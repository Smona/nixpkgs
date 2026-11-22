# Module for enabling embedded development
{ ... }:

{
  den.aspects.embedded-dev.nixos = { user, config, ... }: {
    # Set up sudoless access to MCUs
    users.groups = {
      plugdev = { };
    };
    services.udev.extraRules = builtins.readFile ./69-probe-rs.rules;
    users.users.${user.userName}.extraGroups = [
      "plugdev"
    ];
  };
}
