{ config, pkgs, ... }:

let
  emacs = pkgs.emacs29-pgtk;
  my-emacs = (nixGL ((pkgs.emacsPackagesFor emacs).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.pdf-tools ])));
  nixGL = import ./nixGL.nix { inherit config pkgs; };
in {
  programs.emacs = {
    enable = true;
    package = my-emacs;
  };

  systemd.user.services."emacs" = {
    Unit.Description = "Start emacs at login";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = { ExecStart = "${my-emacs}/bin/emacs"; };
  };

  home.packages = with pkgs; [
    # Python
    python310Packages.debugpy # Required by dap-mode
    nodePackages.pyright

    # Nix
    nil # Required by (nix +lsp)
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
