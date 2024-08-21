{ pkgs, lib, config, inputs, system, ... }:

let user = "mel";
in {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    # Just get... all of the basics from nixpkgs
    bzip2
    coreutils-full
    curl
    diffutils
    file
    findutils
    gawk
    gnugrep
    gnused
    gnutar
    gzip
    less
    man
    netcat
    openssh
    rsync
    inetutils
    tree
    unzip
    watch
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina

  # Set Git commit hash for darwin-version.
  system.configurationRevision =
    inputs.self.rev or inputs.self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;

  ############################
  # Below: stuff added by me #
  ############################

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.${user} = import ./home.nix;
  home-manager.extraSpecialArgs = { inherit inputs system; };

  # Optionally, use home-manager.extraSpecialArgs to pass
  # arguments to home.nix

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;
  system.defaults = {
    NSGlobalDomain = {
      # tap-to-click (not sure how this is different from trackpad.Clicking)
      "com.apple.mouse.tapBehavior" = 1;
      # bump up trackpad speed
      "com.apple.trackpad.scaling" = 2.5;
      # Interval between key repeats when held (lower is faster)
      KeyRepeat = 2;
    };
    trackpad.Clicking = true;
    trackpad.Dragging = true;

    dock = {
      autohide = true;
      persistent-apps = [
        "/Applications/Firefox.app"
        "/Applications/Spotify.app"
        "/opt/homebrew/Cellar/emacs-plus@29/29.4/Emacs.app"
      ];
    };
    # Defaults for which validation hasn't been added to nix-darwin yet
    CustomUserPreferences = {
      NSGlobalDomain = { "com.apple.trackpad.scrolling" = 1.0; };
    };
  };

  # Enable TouchID sudo authentication
  security.pam.enableSudoTouchIdAuth = true;

  users.users.${user} = {
    description = "Mel Bourgeois";
    home = "/Users/${user}";
  };

  nix-homebrew = {
    # Install Homebrew under the default prefix
    enable = true;

    # Apple Silicon Only: Also install Homebrew under the default Intel prefix for Rosetta 2
    enableRosetta = true;

    # User owning the Homebrew prefix
    inherit user;

    # Optional: Declarative tap management
    taps = {
      "homebrew/homebrew-core" = inputs.homebrew-core;
      "homebrew/homebrew-cask" = inputs.homebrew-cask;
      "homebrew/homebrew-bundle" = inputs.homebrew-bundle;
      "d12frosted/homebrew-emacs-plus" = inputs.homebrew-emacs-plus;
    };

    # Optional: Enable fully-declarative tap management
    #
    # With mutableTaps disabled, taps can no longer be added imperatively with `brew tap`.
    mutableTaps = false;
  };

  homebrew = {
    enable = true;

    # Prevent tap management conflicts b/w nix-homebrew & nix-darwin
    # https://github.com/zhaofengli/nix-homebrew/issues/5#issuecomment-1878798641
    taps = builtins.attrNames config.nix-homebrew.taps;

    # Stop being annoyed by Apple "protecting" me
    caskArgs.no_quarantine = true;

    brews = [
      "gcc" # Implicit dependency of emacs-plus --with-native-comp
      {
        # TODO: emacs has to be manually aliased to /Applications for now:
        # osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications"'
        name = "emacs-plus@29";
        args = [
          "with-xwidgets"
          "with-imagemagick"
          "with-native-comp"
          "with-poll"
          "with-modern-doom3-icon"
        ];
      }
      # Emacs dependencies that aren't getting referenced by emacs-plus
      "gnupg"
      "cmake" # Needed to compile vterm
    ];

    casks = [
      "firefox"
      "1password"
      "slack"
      "spotify"
      "middleclick"
      "keybase"
      "kitty"
      "steam"
      "keymapp" # ZSA companion UI, e.g. for flashing
      "proxy-audio-device" # enable using system volume controls w/ external audio interfaces
    ];
  };

  ###########################################################
  # Below: stuff that should come from existing config repo #
  ###########################################################

  # This will additionally add your inputs to the system's legacy channels
  # Making legacy nix commands consistent as well, awesome!
  nix.nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
    config.nix.registry;

  # This will add each flake input as a registry
  # To make nix3 commands consistent with your flake
  nix.registry = lib.mapAttrs (_: value: { flake = value; }) inputs;
}
