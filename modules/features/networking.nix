{ ... }:

{
  den.aspects.networking = {
    nixos = { pkgs, ... }: {
      # Enable networking with networkmanager
      networking.networkmanager = {
        enable = true;
        # Can improve WiFi connection reliability
        wifi.powersave = false;
        wifi.backend = "iwd";
        plugins = with pkgs; [ networkmanager-openvpn ];
      };

      # IWD is much quicker to connect IME.
      networking.wireless.iwd = {
        enable = true;
        # Fixes an issue where luma would randomly disconnect from home wifi
        # by roaming to nowhere.
        settings.Scan.DisableRoamingScan = true;

        settings.General = {
          EnableNetworkConfiguration = true;
          # Another option: roam less aggressively
          # RoamThreshold=-74;
          # RoamThreshold5G=-79;
        };
      };

      # Always use Cloudflare nameservers
      networking.nameservers = [
        "1.1.1.1"
        "1.0.0.1"
      ];
    };
  };
}
