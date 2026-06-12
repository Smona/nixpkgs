# Bootloader, kernel, and silent boot configuration
{ ... }:
{
  flake.nixosModules.boot =
    { pkgs, ... }:
    {
      # Use latest kernel for more stable WiFi connection
      # https://forums.linuxmint.com/viewtopic.php?t=450654
      boot.kernelPackages = pkgs.linuxPackages_6_18;

      boot.kernel.sysctl = {
        # Double the default file watchers for big TS projects
        "fs.inotify.max_user_watches" = 1048576;
      };

      # Bootloader.
      boot.loader = {
        efi = {
          canTouchEfiVariables = true;
          efiSysMountPoint = "/boot/efi";
        };
        grub = {
          enable = true;
          devices = [ "nodev" ];
          efiSupport = true;
          useOSProber = true;
          # Full RAM testing with no bootable flash drive!
          # Working around an issue where the entry doesn't work with a customized efiSysMountPoint
          # WARNING: LLM-generated workaround, validated by this comment:
          # https://discourse.nixos.org/t/options-to-install-and-configure-memtest86plus-seem-inconsistent-and-broken/64130/5
          memtest86.enable = false;
          # Copy the EFI build of memtest86+ onto the ESP.
          # With efiSysMountPoint = "/boot/efi", this lands at /boot/efi/memtest.efi,
          # which is simply /memtest.efi at the root of the FAT32 ESP.
          extraFiles."memtest.efi" = pkgs.memtest86plus.efi;

          extraEntries = ''
            menuentry "Memtest86+" {
                # Locate whichever partition actually contains the file, make it $root.
                search --set=root --no-floppy --file /memtest.efi
                # Chainload the native EFI binary.
                chainloader /memtest.efi
            }
          '';
        };
      };

      boot.plymouth = {
        enable = true;
      };

      # Silent boot
      boot.initrd.verbose = false;
      boot.initrd.systemd.enable = true;
      boot.consoleLogLevel = 0;
      boot.kernelParams = [
        "quiet"
        "udev.log_level=3"
        "splash"
        "boot.shell_on_fail"
        "loglevel=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
        "udev.log_priority=3"
      ];
    };
}
