{ self, inputs, ... }:
{
  den.aspects.emacs.homeManager =
    {
      config,
      pkgs,
      ...
    }:
    {
      smona.wlroots = {
        execStart = [ "emacs" ];
        keyBinds = [
          {
            primaryMod = true;
            key = "c";
            command = [ "~/.emacs.d/bin/org-capture" ];
          }
        ];
      };

      xdg = {
        enable = true;
        configFile."doom" = {
          # TODO: move into directory with this module.
          source = ./doom;
          recursive = true;
        };
        # Pass config from nix into doom emacs configuration
        configFile."doom_vars.el".text = ''
          (setq catppuccin-flavor '${config.catppuccin.flavor})
        '';
      };
      home.file.".authinfo.gpg".source = ./authinfo.gpg;
      home.shellAliases = {
        doom = "LSP_USE_PLISTS=true ~/.emacs.d/bin/doom";
        org-capture = "~/.emacs.d/bin/org-capture";
      };

      home.packages =
        with pkgs;
        [
          # Python
          python3Packages.debugpy # Required by dap-mode
          pyright
          python3 # Required for advanced treemacs features

          # Nix
          nil # Required by (nix +lsp)
          nixfmt # For emacs code formatting

          # C
          cmake-language-server

          # Code Formatters
          prettier
          black
          shfmt

          # Language servers
          # Overridden to enable building on darwin without building emacs from scratch.
          ((emacs-lsp-booster.override { emacs = nil; }).overrideAttrs (prev: {
            doCheck = false;
          }))
          vtsls # VSCode typescript language server
          vscode-langservers-extracted
          dockerfile-language-server
          python3Packages.grip # Required by grip-mode (markdown +grip)
          glslang # glsl-mode intellisense
          tailwindcss-language-server

          # Dirvish stuff
          ffmpegthumbnailer
          gnutar
          unzip
          mediainfo
          imagemagick # improves display of images in emacs

          # org-roam-graph
          graphviz

          # mermaid-mode
          mermaid-cli

          # Assorted Emacs dependencies
          jq # Used by restclient.el
          awscli2 # Used by s3ed
          self.packages.${pkgs.stdenv.hostPlatform.system}.claude-agent-acp # Used by agent-shell

          texliveMedium # needed for latex inline previews & export in org mode
          # TODO: figure out minimal necessary set
          # (texlive.combine { inherit (texlive) scheme-small wrapfig capt-of; }) # pdflatex
        ]
        # TODO: fix ERR_PNPM_PNPM_ENGINE_IDENTITY_UNVERIFIABLE error on linux build so we
        # can use likec4 package on linux
        ++ (lib.optional pkgs.stdenv.hostPlatform.isDarwin
          self.packages.${pkgs.stdenv.hostPlatform.system}.likec4
        );
    };

  # Emacs is installed from nixpkgs on Linux.
  den.aspects.emacs.hmLinux = { pkgs, ... }: {
    programs.emacs = {
      enable = true;
      # TODO: this used to be wrapped with nixgl, next time i use a non-nixos
      # linux i should try using the new method to provide libGL to it.
      package = pkgs.emacs-pgtk;
      extraPackages = epkgs: [
        epkgs.vterm
        epkgs.pdf-tools
      ];
    };
  };

  # Emacs is installed from homebrew on Darwin.
  den.aspects.emacs.darwin = { ... }: {
    nix-homebrew.taps = {
      "d12frosted/homebrew-emacs-plus" = inputs.homebrew-emacs-plus;
    };

    homebrew.brews = [
      "gcc" # Implicit dependency of emacs-plus --with-native-comp
      {
        # TODO: emacs has to be manually aliased to /Applications for now:
        # osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications"'
        name = "emacs-plus@31";
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
      "libtool" # Needed to compile vterm
    ];
  };
}
