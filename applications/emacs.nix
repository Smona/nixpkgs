{ config, pkgs, inputs, system, ... }:

let
  emacs = pkgs.emacs29-pgtk;
  pkgs-latest = import inputs.nixpkgs { inherit system; };
  my-emacs = (config.lib.nixGL.wrap
    ((pkgs.emacsPackagesFor emacs).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.pdf-tools ]))).overrideAttrs (oldAttrs: {
        # Temporarily working around this issue: https://github.com/NixOS/nixpkgs/issues/66706
        # TODO: submit upstream fix
        buildCommand = oldAttrs.buildCommand + ''
          ln -s $emacs/share/emacs $out/share/emacs
        '';
      });
in {
  programs.emacs = {
    enable = true;
    package = my-emacs;
  };

  xdg = {
    enable = true;
    configFile."doom".source = config.lib.file.mkOutOfStoreSymlink ../doom;
  };
  home.file.".authinfo.gpg".source = ../dotfiles/authinfo.gpg;
  home.shellAliases = { doom = "~/.emacs.d/bin/doom"; };

  systemd.user.services."emacs" = {
    Unit.Description = "Start emacs at login";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = { ExecStart = "${my-emacs}/bin/emacs"; };
  };

  home.packages = with pkgs; [
    # Python
    python3Packages.debugpy # Required by dap-mode
    nodePackages.pyright

    # Nix
    nil # Required by (nix +lsp)
    nixfmt # For emacs code formatting

    # C
    cmake-language-server

    # Rust
    rust-analyzer
    rustfmt
    clippy

    # Code Formatters
    nodePackages.prettier
    black
    shfmt

    # Language servers
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.dockerfile-language-server-nodejs
    pkgs-latest.nodePackages.graphql-language-service-cli
    python310Packages.grip # Required by grip-mode (markdown +grip)

    # Dirvish stuff
    ffmpegthumbnailer
    gnutar
    unzip
    mediainfo
    imagemagick # improves display of images in emacs

    # org-roam-graph
    graphviz

    # mermaid-mode
    nodePackages.mermaid-cli

    # Assorted Emacs dependencies
    wakatime
    jq # Used by restclient.el
  ];
}
