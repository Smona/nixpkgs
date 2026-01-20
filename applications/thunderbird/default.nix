{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.thunderbird = {
    enable = true;
    package = pkgs.thunderbird.override {
      # Declaratively install extensions.
      # To get the extension ID (key):
      # - download the xpi file
      # - extract it (it's a zip file)
      # - find the ID in manifest.json
      extraPolicies.ExtensionSettings = {
        "gconversation@xulforum.org" = {
          installation_mode = "force_installed";
          install_url = "https://addons.thunderbird.net/thunderbird/downloads/file/1038439/thunderbird_conversations-4.3.1-tb.xpi";
        };
        "uBlock0@raymondhill.net" = {
          installation_mode = "force_installed";
          install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/ublock-origin/addon-988489-latest.xpi";
        };
        "autoprofilepicture@astucesweb.fr" = {
          installation_mode = "force_installed";
          install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/auto-profile-picture/addon-988770-latest.xpi";
        };
      };
    };
    profiles = {
      personal = {
        isDefault = true;
        userChrome = builtins.readFile ./userChrome.css;
        userContent = builtins.readFile ./userContent.css;
        accountsOrder = [
          "Personal"
          "Work"
        ];
        # Config reference: https://kb.mozillazine.org/Mail_and_news_settings
        settings = {
          # Automatically enable declarative extensions
          "extensions.autoDisableScopes" = 0;
          # TODO: figure out how to enable catppuccin theme automatically.
          # Enable userChrome/userContent customizations
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          # Default to sort by date descending
          "mailnews.default_sort_type" = 18;
          "mailnews.default_sort_order" = 2;
          # Default to threaded view
          "mailnews.default_view_flags" = 1;
        };
      };
    };
  };
  catppuccin.thunderbird = {
    enable = true;
    profile = "personal";
  };

  accounts = {
    email.accounts = {
      Personal = {
        address = "mason.bourgeois@gmail.com";
        flavor = "gmail.com";
        realName = "Mel Bourgeois";
        primary = true;
        thunderbird.enable = true;
      };
      Work = {
        address = "mel@cobaltai.com";
        flavor = "gmail.com";
        realName = "Mel Bourgeois";
        thunderbird = {
          enable = true;
          settings = id: {
            # Enable HTML in signature
            "mail.identity.id_${id}.htmlSigFormat" = true;
            # Include signature on forwards
            "mail.identity.id_${id}.sig_on_fwd" = true;
            # Reply before the quoted text (gmail style)
            "mail.identity.id_${id}.reply_on_top" = 1;
            # Signature before the quoted text (gmail style)
            "mail.identity.id_${id}.sig_bottom" = false;
          };
        };
        signature = {
          text = (builtins.readFile ./cobalt_signature.html);
          showSignature = "append";
        };
      };
    };

    contact.accounts = {
      "Personal Contacts" = {
        remote = {
          type = "carddav";
          userName = "mason.bourgeois@gmail.com";
          url = "https://www.googleapis.com/carddav/v1/principals/mason.bourgeois@gmail.com/lists/default/";
        };
        thunderbird.enable = true;
      };
      "Work Contacts" = {
        remote = {
          type = "carddav";
          userName = "mel@cobaltai.com";
          url = "https://www.googleapis.com/carddav/v1/principals/mel@cobaltai.com/lists/default/";
        };
        thunderbird.enable = true;
      };
    };

    calendar.accounts = {
      Personal = {
        primary = true;
        remote = {
          type = "caldav";
          url = "https://apidata.googleusercontent.com/caldav/v2/mason.bourgeois@gmail.com/events/";
          userName = "mason.bourgeois@gmail.com";
        };
        thunderbird = {
          enable = true;
          # Catppuccin mocha mauve
          color = "#cba6f7";
        };
      };
      Work = {
        remote = {
          type = "caldav";
          url = "https://apidata.googleusercontent.com/caldav/v2/mel@cobaltai.com/events/";
          userName = "mel@cobaltai.com";
        };
        thunderbird = {
          enable = true;
          # Catppuccin mocha sapphire
          color = "#74c7ec";
        };
      };
      "Cobalt Demos" = {
        remote = {
          type = "caldav";
          url = "https://apidata.googleusercontent.com/caldav/v2/cobaltrobotics.com_tdoj8nukt651bopk8fb1a1j3n4@group.calendar.google.com/events/";
          userName = "mel@cobaltai.com";
        };
        thunderbird = {
          enable = true;
          # Catppuccin mocha red
          color = "#f38ba8";
        };
      };
      "Cobalt Team" = {
        remote = {
          type = "caldav";
          url = "https://apidata.googleusercontent.com/caldav/v2/cobaltrobotics.com_l2c1n5u00sob9e84f3udlokne8@group.calendar.google.com/events/";
          userName = "mel@cobaltai.com";
        };
        thunderbird = {
          enable = true;
          # Catppuccin mocha blue
          color = "#89b4fa";
        };
      };
    };
  };
}
