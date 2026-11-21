# Userspace access to Keychron HID devices for users in the "input" group
# (for sudoless WebHID configuration in the browser).
{ ... }:
{
  den.aspects.keychron.nixos =
    { ... }:
    {
      services.udev.extraRules = ''
        KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="3434", TAG+="uaccess", GROUP="input", MODE="0660"
      '';
    };
}
