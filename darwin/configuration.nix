{
  pkgs,
  lib,
  config,
  inputs,
  system,
  ...
}:

{
  imports = [
    inputs.nix-homebrew.darwinModules.nix-homebrew
    inputs.home-manager.darwinModules.home-manager
    ../common_configuration.nix
  ];

  smona.username = "mel";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
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

  # Set Git commit hash for darwin-version.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;

  ############################
  # Below: stuff added by me #
  ############################

  home-manager.users.${config.smona.username} = import ./home.nix;

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
        "${pkgs.spotify}/Applications/Spotify.app"
        "${pkgs.slack}/Applications/Slack.app"
        "${pkgs.kitty}/Applications/kitty.app"
        "/opt/homebrew/Cellar/emacs-plus@29/29.4/Emacs.app"
      ];
    };
    # Defaults for which validation hasn't been added to nix-darwin yet
    CustomUserPreferences = {
      NSGlobalDomain = {
        "com.apple.trackpad.scrolling" = 1.0;
      };
    };
  };

  # Enable TouchID sudo authentication
  security.pam.enableSudoTouchIdAuth = true;

  users.users.${config.smona.username} = {
    description = "Mel Bourgeois";
    home = "/Users/${config.smona.username}";
  };

  nix-homebrew = {
    # Install Homebrew under the default prefix
    enable = true;

    # Apple Silicon Only: Also install Homebrew under the default Intel prefix for Rosetta 2
    enableRosetta = true;

    # User owning the Homebrew prefix
    user = config.smona.username;

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

    onActivation = {
      # TODO: errors from homebrew on uninstall, programs aren't actually cleaned up.
      # This can likely be fixed by using nixpkgs emacs
      cleanup = "uninstall";
      upgrade = true;
    };

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
      "middleclick"
      "keybase"
      "steam"
      "keymapp" # ZSA companion UI, e.g. for flashing
      "proxy-audio-device" # enable using system volume controls w/ external audio interfaces
      "raycast"
    ];
  };
}
