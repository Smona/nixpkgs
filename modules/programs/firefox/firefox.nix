{ ... }: {
  den.aspects.firefox.homeManager =
    { config, pkgs, ... }:
    {
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

      # This requires declarative extensions, and i already found a theme i like.
      catppuccin.firefox.enable = false;
      programs.firefox =
        let
          appDataDir = "Library/Application Support/org.nixos.firefox";
          isLinux = pkgs.stdenv.hostPlatform.isLinux;
        in
        {
          enable = true;
          # Linux: I like the old default, since the .mozilla directory has to exist anyways
          # it's nice to have one less folder.
          # Darwin: The path must not conflict with the default Firefox install directory due to new
          # security restrictions in MacOS Golden Gate.
          configPath = if isLinux then ".mozilla/firefox" else appDataDir;
          package =
            if isLinux then
              (config.lib.nixGL.wrap config.pkgsCompat.firefox)
            else
              config.pkgsCompat.firefox.override {
                appDataDir = "${config.home.homeDirectory}/${appDataDir}";
              };
          nativeMessagingHosts = if isLinux then [ pkgs.fx-cast-bridge ] else [ ];
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
}
