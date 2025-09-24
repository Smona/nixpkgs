{ config, pkgs, ... }:

{
  # This requires declarative extensions, and i already found a theme i like.
  catppuccin.firefox.enable = false;
  programs.firefox = {
    package = (config.lib.nixGL.wrap config.pkgsCompat.firefox);
    nativeMessagingHosts = [ pkgs.fx_cast_bridge ];
    profiles = {
      Smona = {
        settings = {
          # Disable pointer lock warning popup
          "pointer-lock-api.warning.timeout" = 0;
          "general.smoothScroll" = true;
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
}
