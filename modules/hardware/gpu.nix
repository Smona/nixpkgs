# NixOS modules for graphical systems with discrete GPUs.
{ ... }:
let
  # Shared configuration for all GPU types
  common = {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };
  };
in
{
  flake.nixosModules.amdgpu =
    { pkgs, ... }:
    {
      imports = [ common ];

      # Load AMD driver for Xorg and Wayland
      boot.initrd.kernelModules = [ "amdgpu" ];
      # fix occasional green artifacts in Hyprland:
      # https://github.com/hyprwm/Hyprland/issues/2385
      boot.kernelParams = [ "amdgpu.dcdebugmask=0x10" ];
      services.xserver.videoDrivers = [ "amdgpu" ];

      # Radeon GPU
      systemd.services.lact = {
        description = "AMDGPU Control Daemon";
        after = [ "multi-user.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${pkgs.lact}/bin/lact daemon";
        };
        enable = true;
      };
      hardware.amdgpu.overdrive.enable = true;

      environment.systemPackages = [
        pkgs.lact
        pkgs.amdgpu_top
      ];
    };

  flake.nixosModules.nvidia =
    { config, ... }:
    {
      imports = [ common ];

      # Load nvidia driver for Xorg and Wayland
      services.xserver.videoDrivers = [ "nvidia" ];

      hardware.nvidia = {
        # Modesetting is required.
        modesetting.enable = true;

        # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
        # Enable this if you have graphical corruption issues or application crashes after waking
        # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
        # of just the bare essentials.
        powerManagement.enable = false;

        # Fine-grained power management. Turns off GPU when not in use.
        # Experimental and only works on modern Nvidia GPUs (Turing or newer).
        powerManagement.finegrained = false;

        # Use the NVidia open source kernel module (not to be confused with the
        # independent third-party "nouveau" open source driver).
        # Support is limited to the Turing and later architectures. Full list of
        # supported GPUs is at:
        # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
        # Only available from driver 515.43.04+
        # Currently alpha-quality/buggy, so false is currently the recommended setting.
        open = false;

        # Enable the Nvidia settings menu,
        # accessible via `nvidia-settings`.
        nvidiaSettings = true;

        # Optionally, you may need to select the appropriate driver version for your specific GPU.
        # TODO: switch back to stable once on FF 130, which supports explicit sync:
        # https://github.com/NixOS/nixpkgs/pull/313440
        package = config.boot.kernelPackages.nvidiaPackages.production;
      };
    };
}
