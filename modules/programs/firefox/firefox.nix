{ ... }: {
  den.aspects.firefox = {
    homeManager =
      { config, pkgs, ... }:
      {
        # This requires declarative extensions, and i already found a theme i like.
        catppuccin.firefox.enable = false;
        programs.firefox = {
          enable = true;
          profiles = {
            Smona = {
              settings = {
                # Disable pointer lock warning popup
                "pointer-lock-api.warning.timeout" = 0;

                # Fix "Back to Close WE" extension, at the cost of allowing history stuffing.
                "browser.navigation.requireUserInteraction" = false;

                # Enable browser toolbox for inspecting browser chrome and developing userChrome.css
                "devtools.chrome.enabled" = true;
                "devtools.debugger.remote-enabled" = true;

                # Disable GTK native emoji picker, since its hotkey (Ctrl+.) conflicts with the
                # 1password browser extension.
                "widget.gtk.native-emoji-dialog" = false;

                # Betterfox Smoothfox natural smooth scrolling v3:
                # https://github.com/yokoffing/Betterfox/blob/main/Smoothfox.js
                "general.smoothScroll" = true;
                "apz.overscroll.enabled" = true;
                "general.smoothScroll.msdPhysics.continuousMotionMaxDeltaMS" = 12;
                "general.smoothScroll.msdPhysics.enabled" = true;
                "general.smoothScroll.msdPhysics.motionBeginSpringConstant" = 600;
                "general.smoothScroll.msdPhysics.regularSpringConstant" = 650;
                "general.smoothScroll.msdPhysics.slowdownMinDeltaMS" = 25;
                "general.smoothScroll.msdPhysics.slowdownMinDeltaRatio" = "2";
                "general.smoothScroll.msdPhysics.slowdownSpringConstant" = 250;
                "general.smoothScroll.currentVelocityWeighting" = "1";
                "general.smoothScroll.stopDecelerationWeighting" = "1";

                "mousewheel.default.delta_multiplier_y" = 200;
                "mousewheel.default.delta_multiplier_x" = 200;

                # Selected Peskyfox
                #
                # SECTION: AI
                # PREF: AI default controls
                # [1] https://blog.mozilla.org/en/firefox/how-to-use-ai-controls/
                "browser.ai.control.default" = "blocked";
                "browser.ml.enable" = false;
                # PREF: Translations
                # user_pref("browser.ai.control.translations", "blocked");
                # PREF: Image alt text in Nightly PDF viewer
                #user_pref("browser.ai.control.pdfjsAltText", "blocked");
                # PREF: Tab group suggestions
                # [1] https://support.mozilla.org/kb/how-use-ai-enhanced-tab-groups
                #user_pref("browser.ai.control.smartTabGroups", "blocked");
                "browser.tabs.groups.smart.enabled" = false;
                # PREF: Key points in link previews
                #user_pref("browser.ai.control.linkPreviewKeyPoints", "blocked");
                "browser.ml.linkPreview.enabled" = false;
                # PREF: Chatbot in sidebar
                #user_pref("browser.ai.control.sidebarChatbot", "blocked");
                "browser.ml.chat.enabled" = false;
                "browser.ml.chat.menu" = false;

                # SECTION: FULLSCREEN NOTICE
                # PREF: remove fullscreen delay
                "full-screen-api.transition-duration.enter" = "0 0"; # default=200 200
                "full-screen-api.transition-duration.leave" = "0 0"; # default=200 200
                # PREF: disable fullscreen notice
                # [NOTE] Adjust to a sensible value, like 1250, if you have security concerns.
                # I don't, I think I can handle some unexpected fullscreen
                "full-screen-api.warning.timeout" = 0; # default=3000; alt=1250
                "full-screen-api.warning.delay" = -1; # default=500

                # PREF: show all matches in Findbar
                "findbar.highlightAll" = true;

                # Don't show warning when opening about:config
                "browser.aboutConfig.showWarning" = false;
                "browser.toolbars.bookmarks.visibility" = "newtab";
                # Show history & tabs before search suggestions
                "browser.urlbar.showSearchSuggestionsFirst" = false;
                # Restore the previous sesssion on startup
                "browser.startup.page" = 3;
                "services.sync.username" = "mason.bourgeois@gmail.com";
                # Hide the sharing popup window indicator.
                # On wayland it shows up as a window in alt-tab, and it's kind of
                # annoying even on platforms where it's more properly handled.
                "privacy.webrtc.legacyGlobalIndicator" = false;
                # Enable hardware video acceleration
                # https://wiki.archlinux.org/title/firefox#Hardware_video_acceleration
                "media.ffmpeg.vaapi.enabled" = true;
                "media.hardware-video-decoding.enabled" = true;
                "media.hardware-video-decoding.force-enabled" = true;
                # I tend to like a larger font size for web content compared to the base
                # OS font size
                "font.size.systemFontScale" = 110;
                # Disable ads
                "browser.newtabpage.activity-stream.showSponsored" = false;
                "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
                # Enable userChrome.css
                "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
              };
              userChrome = builtins.readFile ./firefox.css;
            };
          };
        };
      };

    hmDarwin = { config, ... }: {
      programs.firefox = {
        # The path must not conflict with the default Firefox install directory due to new
        # security restrictions in MacOS Golden Gate.
        configPath = "Library/Application Support/org.nixos.firefox";
        # Installed with homebrew on darwin, see below
        package = null;
        # Declarative nativeMessagingHosts seem to require the wrapper, which breaks
        # application signing.
        nativeMessagingHosts = [ ];
      };

      # Point homebrew firefox at nix-managed profiles directory
      launchd.agents.firefox-env = {
        enable = true;
        config = {
          ProgramArguments = [
            "/bin/sh"
            "-c"
            ''
              /bin/launchctl setenv MOZ_APP_DATA "${config.home.homeDirectory}/${config.programs.firefox.configPath}"
              /bin/launchctl setenv MOZ_LEGACY_PROFILES 1
            ''
          ];
          RunAtLoad = true;
        };
      };
    };

    darwin = {
      # Firefox must be installed via homebrew on Darwin so that the binary is properly
      # signed and 1password desktop will allow us to integrate with it.
      homebrew.casks = [ "firefox" ];
      # Pin to dock
      system.defaults.dock.persistent-apps = [ "/Applications/Firefox.app" ];
    };

    hmLinux = { pkgs, config, ... }: {
      programs.firefox = {
        # I like the old default, since the .mozilla directory has to exist anyways
        # it's nice to have one less folder.
        configPath = ".mozilla/firefox";
        package = (config.lib.nixGL.wrap config.pkgsCompat.firefox);
        nativeMessagingHosts = [ pkgs.fx-cast-bridge ];
      };

      smona.wlroots = {
        execStart = [ "firefox" ];
        keyBinds = [
          {
            primaryMod = true;
            key = "b";
            command = [ "firefox" ];
          }
        ];
      };
    };
  };
}
