{ config, lib, pkgs, ... }:

let
  emacs = pkgs.emacsPgtk;
  nixGL = import ./nixGL.nix { inherit config pkgs; };
in {
  programs.emacs = {
    enable = true;
    package = (nixGL ((pkgs.emacsPackagesFor emacs).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.pdf-tools ])));
  };

  home.packages = with pkgs; [
    # Python
    python310Packages.debugpy # Required by dap-mode
    nodePackages.pyright

    # Nix
    rnix-lsp # Required by (nix +lsp)
    nixfmt # For emacs code formatting

    # C
    cmake-language-server

    # Code Formatters
    nodePackages.prettier
    black
    shfmt

    # Language servers
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.dockerfile-language-server-nodejs
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
  ];
}
