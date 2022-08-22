{ config, lib, pkgs, ... }:

let
  settings = import ../settings.nix;
  wrapWithNixGL = import ./nixGL.nix pkgs;
in {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland.override {
      extraNativeMessagingHosts = [ pkgs.fx_cast_bridge ];
    };
    profiles = {
      Smona = {
        settings = {
          "browser.toolbars.bookmarks.visibility" = "newtab";
          "services.sync.username" = "mason.bourgeois@gmail.com";
          # Enable hardware video acceleration
          # https://wiki.archlinux.org/title/firefox#Hardware_video_acceleration
          "media.ffmpeg.vaapi.enabled" = true;
          "media.hardware-video-decoding.enabled" = true;
          "media.hardware-video-decoding.force-enabled" = true;
        };
      };
    };
  };

  # Provide access to drivers so hardware acceleration works on non-NixOS
  xdg.desktopEntries.firefox = {
    # TODO: figure out how to inherit attributes from the base desktop item
    name = "Firefox";
    genericName = "Web Browser";
    exec = wrapWithNixGL "firefox %U";
    categories = [ "Network" "WebBrowser" ];
    icon = "firefox";
    mimeType = [
      "text/html"
      "text/xml"
      "application/xhtml+xml"
      "application/vnd.mozilla.xul+xml"
      "x-scheme-handler/http"
      "x-scheme-handler/https"
      "x-scheme-handler/ftp"
    ];
    type = "Application";
  };

  home.sessionVariables = {
    # Enable smooth scrolling and zooming in firefox.
    # Source: https://www.reddit.com/r/firefox/comments/l5a9ez/comment/gktzijc/
    # Disabled for now to see if firefox-wayland is sufficient TODO: confirm on pure system
    # MOZ_USE_XINPUT2 = 1;
    # MOZ_USE_WAYLAND = 1;
  };

}
