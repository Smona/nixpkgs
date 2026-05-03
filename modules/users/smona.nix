# My standard Home Manager configurations
{ inputs, self, ... }:
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
        inputs.dCachix.homeManagerModules.declarative-cachix
        inputs.catppuccin.homeModules.catppuccin
        self.homeModules.fonts
        self.homeModules.git
        self.homeModules.vim
        ../../applications/shell.nix
        ../../applications/tmux.nix
        ../../applications/emacs.nix
        ../../applications/agents.nix
        ../../applications/common_gui.nix
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
        caches.cachix = [
          {
            name = "nix-community";
            sha256 = "sha256:0m6kb0a0m3pr6bbzqz54x37h5ri121sraj1idfmsrr6prknc7q3x";
          }
        ];

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
        };

        home.language.base = "en_US.UTF-8";

        home.packages = with pkgs; [
          cachix

          ## NodeJS
          nodejs
          yarn
          prettier
        ];

        home.sessionVariables =
          let
            editor = "vim";
          in
          {
            RIPGREP_CONFIG_PATH = "${../../dotfiles/rg.conf}";
            VISUAL = editor;
            EDITOR = editor;
            GIT_EDITOR = editor;
          };

        # Enable nix CLI helper
        programs.nh = {
          enable = true;
          flake = "$HOME/.config/nixpkgs";
        };

        # Dotfiles
        xdg = {
          enable = true;
          configFile = {
            ".curlrc".source = ../../dotfiles/curlrc;
          };
        };
        home.file.".inputrc".source = ../../dotfiles/inputrc;

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
  # Linux-only home-manager configuration shared across personal computers.
  flake.homeModules.linux-pc =
    {
      config,
      pkgs,
      ...
    }:
    {
      imports = [
        common
        ../../applications/gui.nix
        ../../desktops/gnome
        ../../desktops/wlroots
        ../../peripherals/logitech.nix
      ];

      # Home Manager needs a bit of information about you and the
      # paths it should manage.
      home.homeDirectory = "/home/${config.home.username}";
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
      services.gpg-agent.pinentryPackage = pkgs.pinentry-gnome3;

      # Automatically start/stop/restart services when their configuration changes
      systemd.user.startServices = "sd-switch";

      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      home.stateVersion = "22.05";
    };

  # Darwin-only home-manager configuration shared across personal computers.
  flake.homeModules.darwin-pc =
    { pkgs, ... }:
    {
      imports = [ common ];

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
