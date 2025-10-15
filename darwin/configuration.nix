{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  imports = [
    inputs.nix-homebrew.darwinModules.nix-homebrew
    inputs.home-manager.darwinModules.home-manager
    ../common_configuration.nix
  ];

  smona.username = "mel";

  # Enable building linux packages with matching CPU architecture automatically on darwin.
  nix = {
    linux-builder = {
      enable = true;
      systems = [ "aarch64-linux" ];
      maxJobs = 4;
    };

    # This line is a prerequisite
    settings.trusted-users = [ "@admin" ];

    # Not strictly necessary, but this will reduce your disk utilization
    # builders-use-substitutes = true;
  };

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

    # https://stackoverflow.com/questions/57591432/gpg-signing-failed-inappropriate-ioctl-for-device-on-macos-with-maven
    pinentry_mac
  ];

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

  system.primaryUser = config.smona.username;
  home-manager.users.${config.smona.username} = import ./home.nix;

  # Set default apps for media types
  system.activationScripts.setDefaultApps = {
    text = ''
      ${pkgs.duti}/bin/duti -s org.nixos.thunderbird mailto
    '';
  };

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

    # Make finder actually somewhat useful
    finder = {
      ShowPathbar = true;
      ShowStatusBar = true;
      # Default search to the current folder
      FXDefaultSearchScope = "SCcf";
    };

    dock = {
      autohide = true;
      # this is distracting and not super helpful for me
      show-recents = false;
      persistent-apps =
        let
          # So cool that you can reference this here!
          # I found it by inspecting the structure of `config` with:
          # nix repl .#
          # which adds all the top-level outputs to the REPL as variables :muscle:
          spotifyPackage =
            builtins.head
              config.home-manager.users.${config.smona.username}.programs.spicetify.createdPackages;
          thunderbirdPackage =
              config.home-manager.users.${config.smona.username}.programs.thunderbird.package;
        in
        [
          "/Applications/Firefox.app"
          "${thunderbirdPackage}/Applications/Thunderbird.app"
          "${spotifyPackage}/Applications/Spotify.app"
          "${pkgs.slack}/Applications/Slack.app"
          "${pkgs.kitty}/Applications/kitty.app"
          "/opt/homebrew/opt/emacs-plus/Emacs.app"
        ];
    };

    # Defaults for which validation hasn't been added to nix-darwin yet
    CustomUserPreferences = {
      NSGlobalDomain = {
        "com.apple.trackpad.scrolling" = 1.0;
      };
      "com.apple.symbolichotkeys".AppleSymbolicHotKeys = {
        # Disable ctrl+space for switching languages so it doesn't interfere with autocomplete.
        "60".enabled = false;
        "61".enabled = false;
      };
    };
  };

  # Enable TouchID sudo authentication
  security.pam.services.sudo_local.touchIdAuth = true;

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
      autoUpdate = true;
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
        name = "emacs-plus@30";
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
      "caffeine"
      "chromium"
      "floorp"
      "1password"
      "middleclick"
      "keybase"
      "steam"
      "keymapp" # ZSA companion UI, e.g. for flashing
      "proxy-audio-device" # enable using system volume controls w/ external audio interfaces
      "raycast"
      "dozer"
      "logitech-options"
    ];
  };
}
