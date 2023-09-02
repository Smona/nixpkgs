{ config, lib, pkgs, ... }:

let nixGL = import ./nixGL.nix { inherit config pkgs; };
in {
  programs.firefox = {
    package = (nixGL (pkgs.firefox.override {
      extraNativeMessagingHosts = [ pkgs.fx_cast_bridge ];
    }));
    profiles = {
      Smona = {
        settings = {
          "browser.toolbars.bookmarks.visibility" = "newtab";
          # Show history & tabs before search suggestions
          "browser.urlbar.showSearchSuggestionsFirst" = false;
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
