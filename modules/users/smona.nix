# My standard user configurations
{
  inputs,
  den,
  self,
  lib,
  ...
}:
let
  # Home Manager configuration shared between Linux & Darwin.
  # As much stuff as possible should go in here, but some things are
  # necessarily different between the two platforms.
  common =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      imports = [
        inputs.catppuccin.homeModules.catppuccin
        self.homeModules.desktop
        self.homeModules.fonts
        self.homeModules.git
        self.homeModules.tmux
        self.homeModules.vim
        self.homeModules.shells
        self.homeModules.emacs
        self.homeModules.vicinae
      ];

      options = with lib; {
        pkgsCompat = mkOption {
          type = types.pkgs;
          default = pkgs;
          description = ''
            A version of nixpkgs which lags behind the main package set, to preserve
            compatibility with non-NixOS installs on distributions that have slower
            release cycles (e.g. Ubuntu).

            This is especially necessary for packages which integrate closely with the
            system-supplied desktop environment (e.g. gnome extensions), but can come
            up in other cases as well.
          '';
        };
      };

      config = {
        catppuccin.enable = true;
        catppuccin.flavor = "mocha";

        # Configuration of the nix CLI
        # https://nixos.org/manual/nix/stable/command-ref/conf-file.html
        nix.package = lib.mkDefault pkgs.nix;
        nix.settings = {
          # Massively reduce disk usage by hard linking any duplicate files
          # to a single location on disk.
          # FIXME: only enable this outside of nix-darwin & nixos.
          # broken on darwin: https://github.com/NixOS/nix/issues/7273
          # auto-optimise-store = true;
          # Enable nix command and nix flakes
          experimental-features = "nix-command flakes";
          # Always show failure traces
          show-trace = true;
          substituters = [
            "https://cache.nixos.org"
            "https://nix-community.cachix.org"
            "https://vicinae.cachix.org"
          ];
          trusted-public-keys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "vicinae.cachix.org-1:1kDrfienkGHPYbkpNj1mWTr7Fm1+zcenzgTizIcI3oc="
          ];
          netrc-file = "${config.home.homeDirectory}/.config/nix/netrc";
        };

        home.language.base = "en_US.UTF-8";

        home.packages = with pkgs; [
          cachix

          ## NodeJS
          nodejs
          yarn
          prettier
        ];

        # Set vim as default editor for quick stuff
        home.sessionVariables =
          let
            editor = "vim";
          in
          {
            VISUAL = editor;
            EDITOR = editor;
            GIT_EDITOR = editor;
          };

        # Enable nix CLI helper
        programs.nh = {
          enable = true;
          flake = "$HOME/.config/nixpkgs";
        };

        programs.ssh = {
          enable = true;
          includes = [ "~/.ssh/config.local" ];
        };

        # Services
        services.syncthing.enable = true;

        # Let Home Manager install and manage itself.
        programs.home-manager.enable = true;
      };
    };
in
{
  # alternate usernames
  den.aspects.mel.includes = [ den.aspects.smona ];
  den.aspects.smona = { host, ... }: {
    user = {
      description = "Mel Bourgeois";
    };

    includes =
      with den.aspects;
      [
        gaming
        realtime-audio

        # peripherals
        keychron
        lilnasx
      ]
      ++ (lib.lists.optional (lib.systems.elaborate host.system).isLinux den.aspects.linux-pc);
  };

  # Linux-only home-manager configuration shared across personal computers.
  den.aspects.linux-pc = {
    homeManager = { config, pkgs, ... }: {
      imports = [
        common
        self.homeModules.linux-gui
        self.homeModules.pi # Currently unsupported on Darwin because bubblewrap doesn't work there, and our sandboxing mechanism requires it.
      ];

      fonts.fontconfig.enable = true;
      targets.genericLinux.enable = true;

      home.packages = with pkgs; [
        # Programming languages
        gcc # required to compile some packages, e.g. emacsqlite

        # system tools
        dmidecode
        wireplumber
        appimage-run
        usbutils
        pciutils
      ];

      # Set up the 1password SSH agent
      # TODO: fix
      # programs.ssh.extraConfig = "IdentityAgent ~/.1password/agent.sock";

      # Services
      services.keybase.enable = true;

      programs.gpg.enable = true;

      services.gpg-agent.enable = true;
      # Source: https://discourse.nixos.org/t/cant-get-gnupg-to-work-no-pinentry/15373/2
      services.gpg-agent.pinentry.package = pkgs.pinentry-gnome3;

      # Automatically start/stop/restart services when their configuration changes
      systemd.user.startServices = "sd-switch";
    };
  };

  # Darwin-only home-manager configuration shared across personal computers.
  flake.homeModules.darwin-pc =
    { pkgs, ... }:
    {
      imports = [
        common
        self.homeModules.darwin-gui
      ];

      graphical = true;
      roles = {
        work = true;
      };

      home.packages = with pkgs; [
        colima
        vlc-bin
      ];

      # Set up the 1password SSH agent
      programs.ssh.extraConfig = ''
        IdentityAgent "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
      '';

      # You should not change this value, even if you update Home Manager. If you do
      # want to update the value, then make sure to first check the Home Manager
      # release notes.
      home.stateVersion = "24.05"; # Please read the comment before changing.
    };
}
